use super::*;

fn body(insts: &[Inst]) -> Vec<Word> {
    utils::encode(insts)
}

fn str_to_w(str: &str) -> Vec<Word> {
    let bytes = str.as_bytes();
    let mut words: Vec<Word> = bytes.iter().map(|byte| *byte as Word).collect();
    words.push(0);
    words
}

struct Compiler {
    buf: [Word; N_ADDRESSES],
    pos: usize,
}

impl Compiler {
    fn new(start: usize) -> Self {
        Self {
            buf: [0u16; N_ADDRESSES],
            pos: start,
        }
    }

    fn pos(&self) -> usize {
        self.pos
    }

    fn write(&mut self, word: Word) {
        self.buf[self.pos] = word;
        self.pos += 1;
    }

    fn emit(&mut self, inst: Inst) {
        for word in utils::encode(&[inst]) {
            self.write(word);
        }
    }

    fn body_len_p(&self, base_p: Word) -> Word {
        base_p + 2 + self.buf[base_p as usize + 1]
    }

    fn body_p(&self, base_p: Word) -> Word {
        base_p + 3 + self.buf[base_p as usize + 1]
    }

    fn compile_call(&self, base_p: Word) -> Vec<Word> {
        let target = self.body_p(base_p);
        let target_len_p = base_p + 2 + self.buf[base_p as usize + 1];
        let target_len = self.buf[target_len_p as usize];

        if target_len < INLINE_MAX_LEN {
            let mut ret = vec![];
            let mut k = 0;
            while k < target_len - 1 {
                ret.push(self.buf[(target + k) as usize]);
                k += 1;
            }
            ret
        } else {
            utils::encode(&[Inst::Call(target)])
        }
    }

    // layout: link namelen name bodylen body ret
    fn dict_entry(&mut self, link: Word, name: &str, body: &[Word]) -> Word {
        let base = self.pos;
        let name_bytes = name.as_bytes();
        self.write(link);
        self.write(name_bytes.len() as Word);
        for byte in name_bytes {
            self.write(*byte as Word);
        }
        self.write(body.len() as Word + 1);
        for &word in body {
            self.write(word);
        }
        let ret = *utils::encode(&[Inst::Ret()]).first().unwrap();
        self.write(ret);
        base as Word
    }

    // layout: link namelen name datalen data
    fn dict_data(&mut self, link: Word, name: &str, data: &[Word]) -> Word {
        let base = self.pos;
        let name = str_to_w(name);
        self.write(link);
        self.write(name.len() as Word);
        for word in &name {
            self.write(*word);
        }
        self.write(data.len() as Word);
        for &word in data {
            self.write(word);
        }
        base as Word
    }

    fn patch(&mut self, base_p: Word, value: Word) {
        self.buf[self.body_p(base_p) as usize] = value;
    }

    fn jump(&mut self, base_p: Word) {
        let target = self.body_p(base_p);
        self.emit(Inst::Jump(target));
    }

    fn jump_at(&mut self, pos: usize, base_p: Word) {
        let saved = self.pos;
        self.pos = pos;
        self.jump(base_p);
        self.pos = saved;
    }

    fn as_bytes(&self) -> Vec<u8> {
        self.buf
            .iter()
            .flat_map(|word| word.to_be_bytes())
            .collect()
    }
}

const INLINE_MAX_LEN: u16 = 3;

pub fn gen_kernel_image(path: &Path) {
    println!("Generating kernel...");
    let mut c = Compiler::new(0x8);
    let mut link: Word = 0;

    link = c.dict_entry(link, "halt", &body(&[Inst::Halt()]));
    let d_halt = link;
    link = c.dict_entry(link, "@", &body(&[Inst::Load()]));
    link = c.dict_entry(link, "!", &body(&[Inst::Store()]));
    link = c.dict_entry(link, "+", &body(&[Inst::Add()]));
    link = c.dict_entry(link, "-", &body(&[Inst::Subtract()]));
    link = c.dict_entry(link, "and", &body(&[Inst::And()]));
    link = c.dict_entry(link, "or", &body(&[Inst::Or()]));
    link = c.dict_entry(link, "xor", &body(&[Inst::Xor()]));
    link = c.dict_entry(link, "sll", &body(&[Inst::ShiftLogicalL()]));
    link = c.dict_entry(link, "slr", &body(&[Inst::ShiftLogicalR()]));
    link = c.dict_entry(link, "sar", &body(&[Inst::ShiftArithR()]));
    link = c.dict_entry(link, "drop", &body(&[Inst::Drop()]));
    link = c.dict_entry(link, "dup", &body(&[Inst::Get(0)]));
    link = c.dict_data(link, "d/nextp", &[0]);
    let d_nextp = link;
    link = c.dict_data(link, "d/latestp", &[0]);
    let d_latestp = link;
    link = c.dict_entry(link, "d/find", &[]);
    link = c.dict_data(link, "sys/hello", &str_to_w("ATK16 kernel\r\n"));
    let d_sys_hello = link;
    link = c.dict_entry(
        link,
        "sput",
        &body(&[
            Inst::Literal(MMIO_SECTION + MMIO_SERIAL_WRITE),
            Inst::Store(),
        ]),
    );
    let d_sput = link;

    let d_sys_print_hello_loop = c.pos();
    link = c.dict_entry(
        link,
        "sys/print-hello/loop",
        &body(&[
            Inst::Get(0),
            Inst::Load(),
            Inst::Get(0),
            Inst::RetIfZero(),
            Inst::Call(c.body_p(d_sput) as Word),
            Inst::Literal(1),
            Inst::Add(),
            Inst::Jump(c.body_p(d_sys_print_hello_loop as Word)),
        ]),
    );
    let d_sys_print_hello = c.pos();
    link = c.dict_entry(
        link,
        "sys/print-hello",
        &body(&[
            Inst::Literal(c.body_p(d_sys_hello)),
            Inst::Call(c.body_p(d_sys_print_hello_loop as Word)),
            Inst::Drop(),
        ]),
    );
    link = c.dict_entry(
        link,
        "sys/init",
        &body(&[Inst::Call(d_sys_print_hello as Word), Inst::Halt()]),
    );
    let d_sys_init = link;

    // patch dict variables to correct values
    let nextp_val = c.pos() as Word;
    c.patch(d_nextp, nextp_val);
    c.patch(d_latestp, link);

    // add entrypoint jump to start of image
    c.jump_at(0, d_sys_init);

    let bytes = c.as_bytes();
    assert!(bytes.len() == N_ADDRESSES * 2);

    let usage = 100f64 * (c.pos() as f64) / (N_ADDRESSES as f64);
    println!(
        "Writing kernel image to {}, FRAM usage {:.2}%",
        path.display(),
        usage
    );
    std::fs::write(path, bytes).unwrap();
}

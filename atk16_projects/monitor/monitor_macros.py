from atk16_asm.asm_ops import *

expansions: dict[str, Callable[..., ExpandResult]] = {}
def register_macro(func):
  expansions[func.__name__] = func
  return func

@register_macro
def m_newline(r1: str, r2: str) -> ExpandResult:
  return [
    *expand_ldi("text_cursor_p", r1),
    *expand_ldr(r1, r1),
    *expand_ldr(r1, r2),
    *expand_slri(r2, "6", r2), # cursor = cursor / 64
    *expand_addi(r2, "1", r2), # cursor = cursor + 1
    *expand_slli(r2, "6", r2), # cursor = cursor * 64
    *expand_str(r2, r1),
  ]

@register_macro
def m_put_char(char_r: str, r1: str, r2: str) -> ExpandResult:
  return [
    *expand_ldi("text_cursor_p", r1),
    *expand_ldr(r1, r1),
    *expand_ldr(r1, r2),
    *expand_str(char_r, r2),
    *expand_addi(r2, "1", r2),
    *expand_str(r2, r1),
  ]

def m_set_interrupt_state(r1: str, r2: str, value: int) -> ExpandResult:
  return [
    *expand_ldi("vt_iset_addr", r1),
    *expand_ldr(r1, r1),
    *expand_ldi(value, r2),
    *expand_str(r2, r1),
  ]

@register_macro
def m_set_critical(r1: str, r2: str) -> ExpandResult:
  return m_set_interrupt_state(r1, r2, 1)

@register_macro
def m_clear_critical(r1: str, r2: str) -> ExpandResult:
  return m_set_interrupt_state(r1, r2, 0)

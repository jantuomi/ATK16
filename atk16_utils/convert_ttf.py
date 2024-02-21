from PIL import Image, ImageFont, ImageDraw

import sys

if len(sys.argv) != 3:
  print("usage: convert_ttf.py <infile.ttf> <outfile.txt>")
  sys.exit(1)

# Define the font file and size
font_file = sys.argv[1]
out_file = sys.argv[2]
font_size = 8  # 8x8 pixels

# Create a font object
font = ImageFont.truetype(font_file, font_size)


# backspace 0x8, tab 0x9, newline 0xA, space 0x20

# Define the characters to render
#characters = '                                 !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~                          ¡¢£¤¥¦§¨©ª«¬­®¯°±²³´µ¶·¸¹º»¼½¾¿ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞßàáâãäåæçèéêëìíîïðñòóôõö÷øùúûüýþÿ'

def to_char_code(i: int) -> str:
    if i == 10:
        return " "
    return chr(i)

characters = [to_char_code(i) for i in range(0, 256)]


# Open a file to write the bitmaps to
with open(out_file, 'w') as bitmap_file:
    for char_idx, char in enumerate(characters):
        # Create a new image with a white background
        image = Image.new('1', (8, 8), 1)

        # Create a drawing context
        draw = ImageDraw.Draw(image)

        # Draw the character
        draw.text((0, 0), char, font=font, fill=0)

        # Convert the image to a list of 0's and 1's
        pixels = image.getdata()
        bitmap = [1 if pixel == 0 else 0 for pixel in pixels]

        # Write the bitmap to the file
        bitmap_file.write(f'Character {char_idx}: {char}\n')
        for i in range(0, 64, 8):
            row = bitmap[i:i+8]
            bitmap_file.write(''.join(str(bit) for bit in row) + '\n')
        bitmap_file.write('\n')

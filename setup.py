from setuptools import setup, find_packages

setup(
  name='atk16',
  version='0.1',
  packages=find_packages(),
  entry_points={
    'console_scripts': [
      'atk16c=atk16_asm.assembler:main',
      'atk16emu=atk16_emu.cli:main',
    ],
  },
  package_data={
    "atk16_asm": ["builtin_asm/*"],
    "atk16_emu": ["resources/*"],
  },
  install_requires=[
    "getch>=1.0",
    "pygame>=2.5.2",
  ]
)

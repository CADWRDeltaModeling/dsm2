# -*- mode: python -*-

block_cipher = None

a = Analysis(['test_ptm.py'],
             pathex=['/home/djackson/Documents/QEDA/DWR/test_ptm'],
             binaries=[],
             datas=[],
             hiddenimports=["seaborn"],
             hookspath=[],
             runtime_hooks=[],
             excludes=[],
             win_no_prefer_redirects=False,
             win_private_assemblies=False,
             cipher=block_cipher,
             noarchive=False)
a.datas+=Tree("/home/djackson/Documents/QEDA/DWR/test_ptm/data/", prefix="data", excludes=[".DS_Store"])
a.datas+=Tree("/home/djackson/Documents/QEDA/DWR/test_ptm/test_templates/", prefix="test_templates", excludes=[".DS_Store"])
pyz = PYZ(a.pure, a.zipped_data,
             cipher=block_cipher)
exe = EXE(pyz,
          a.scripts,
          [],
          exclude_binaries=True,
          name='test_ptm',
          debug=False,
          bootloader_ignore_signals=False,
          strip=False,
          upx=True,
          console=True )
coll = COLLECT(exe,
               a.binaries,
               a.zipfiles,
               a.datas,
               strip=False,
               upx=True,
               name='test_ptm')

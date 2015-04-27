set print frame-arguments all
#source $HOME/git/rfs/src/tools/gdb-printers/global_gdbinit
python
printersdir = '/home/tdings/git/coreutils'
if printersdir not in sys.path:
  sys.path.insert(0, printersdir)
sys.path.insert (0, "/usr/share/gcc-4.6/python")
end
source /home/tdings/git/coreutils/svtprinters/gdbinit


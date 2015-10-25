abstract AnnaAbs = {
  flags
    startcat = S ;
  cat
    S ;
    Command ;
    Comment ;
  fun
    bot : Comment ;
    anna : Comment ;
    canyou : Command -> Command ;
    please : Command -> Command ;
    tell : Command ;
    op : Command ;
    deop : Command ; 
    s : Command -> S ;
}
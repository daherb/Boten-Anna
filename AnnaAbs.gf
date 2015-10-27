abstract AnnaAbs = {
  flags
    startcat = S ;
  cat
    S ;
    Command ;
    Comment ;
    Name ;
    Bot ; 
  fun
    bot : Bot ;
    anna : Name ;
    canyou : Command -> Command ;
    name : Name -> Command -> Command ;
    please : Command -> Command ;
    tell : Command ;
    op : Command ;
    deop : Command ; 
    commandS : Command -> S ;
    commentS : Comment -> S ;
    botC : Bot -> Comment ;
    nameC : Name -> Comment ;
}
abstract AnnaAbs = {
  flags
    startcat = S ;
  cat
    S ;
    Command ;
    Op;
    Deop;
    Tell;
    Comment ;
    Name ;
    Bot ; 
  fun
    bot : Bot ;
    anna : Name ;
    canyou : Command -> Command ;
    name : Name -> Command -> Command ;
    please : Command -> Command ;
    tell : Tell ;
    op : Op ;
    deop : Deop ; 
    commandS : Command -> S ;
    commentS : Comment -> S ;
    botC : Bot -> Comment ;
    nameC : Name -> Comment ;
    tellC : Tell -> Command ;
    opC : Op -> Command ;
    deopC : Deop -> Command ;
}
abstract Anna = {
  flags
    startcat = S ;
  cat
    S ;
    Command ;
    Op ;
    Deop ;
    Tell ;
    Help ;
    Ping ;
    Comment ;
    Name ;
    Bot ;
    Placeholder ;
  fun
    bot : Bot ;
    anna : Name ;
    canyou : Command -> Command ;
    name : Name -> Command -> Command ;
    please : Command -> Command ;
    tell : Tell ;
    op : Op ;
    deop : Deop ;
    help : Help ;
    ping : Ping ; 
    commandS : Command -> S ;
    commentS : Comment -> S ;
    botC : Bot -> Comment ;
    nameC : Name -> Comment ;
    whoareyouC : Comment ;
    tellC : Tell -> Placeholder -> Command ;
    opC : Op -> Placeholder -> Command ;
    deopC : Deop -> Placeholder -> Command ;
    helpC : Help -> Command ;
    pingC : Ping -> Placeholder -> Command ;
    nick : Placeholder;
    nameP : Name -> Placeholder;
    verboseP : S ; -- When more parameters than expected given
    tellToRcptP : S ; -- Transmit the message
    tellToSndP : S ; -- Confirm transmission
    pingToRcptP : S ; -- Ping someone
    pingToSndP : S ; -- Confirm ping
    accomplishP : S ; -- Annoy user
}
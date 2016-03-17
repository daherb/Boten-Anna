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
    -- Kind of lexicon
    bot : Bot ;
    anna : Name ;
    me : Name ;
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
    -- Possible Comments like mentioning the word bot or the name
    botC : Bot -> Comment ;
    nameC : Name -> Comment ;
    whoareyouC : Comment ;
    -- Possible commands like tell, (de)op, help, and ping
    tellC : Tell -> Placeholder -> Command ;
    opC : Op -> Placeholder -> Command ;
    deopC : Deop -> Placeholder -> Command ;
    helpC : Help -> Command ;
    pingC : Ping -> Placeholder -> Command ;
    nick : Placeholder;
    nameP : Name -> Placeholder;
    -- Other phrases
    verboseP : S ; -- When more parameters than expected given
    tellToRcptP : S ; -- Transmit the message
    tellToSndP : S ; -- Confirm transmission
    pingToRcptP : S ; -- Ping someone
    pingToSndP : S ; -- Confirm ping
    accomplishP : S ; -- Annoy user
}
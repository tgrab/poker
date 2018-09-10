(def-url-0 "/reloadgame"
    (read-game (game-id +game-log+))
    (awhen (get-parameter "agent") (setq +agent-name+ it))
    (main-page
      ((:a href "/game") "Game reloaded")))


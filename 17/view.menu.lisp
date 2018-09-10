(defun view-menu()
    (with-html-output-to-string (*standard-output*)
	(:div :id "bluemenu" :class "bluetabs"
	 (:ul    
	    "<li><a href='/'> Start Page</a>"
	    "<li><a href='/game'> Current game</a>"
	    "<li> <a rel='menu_1'>Search</a>"
	    "<li><a href='l1.htm' rel='menu_2'> History 2007</a>"
	    "<li><a href='/show_short_memory' rel='menu_3'> Parameters </a>"
		))

	(:div :id "menu_1" :class "dropmenudiv_b"
	  "<a href='/calendar'>Select games </a>"
	  "<a href='/choose_selected_games'> Choose from selected games</a>"
	  )

	(:div :id "menu_2" :class "dropmenudiv_b"
	"<a href='/calendar?kind=3&month=6&year=2007'> Omaha July</a>" 
	"<a href='/calendar?kind=2&month=6&year=2007'> Holdem Fixed July</a>"
	"<a href='/calendar?kind=3&month=6&year=2007'> Omaha June</a>" 
	"<a href='/calendar?kind=2&month=6&year=2007'> Holdem Fixed June</a>" 
	"<a href='/calendar?kind=3&month=5&year=2007'> Omaha May</a>" 
	"<a href='/calendar?kind=2&month=5&year=2007'> Holdem Fixed May</a>" 
	"<a href='/calendar?kind=3&month=4&year=2007'> Omaha April</a>" 
	"<a href='/calendar?kind=2&month=4&year=2007'> Holdem Fixed April</a>" 
	"<a href='/calendar?kind=3&month=3&year=2007'> Omaha March</a>" 
	"<a href='/calendar?kind=2&month=3&year=2007'> Holdem Fixed March</a>" 
	"<a href='/calendar?kind=3&month=2&year=2007'> Omaha February</a>" 
	"<a href='/calendar?kind=2&month=2&year=2007'> Holdem Fixed February</a>" 
	"<a href='/calendar?kind=3&month=1&year=2007'> Omaha January</a>" 
	"<a href='/calendar?kind=2&month=1&year=2007'> Holdem Fixed January</a>" 

		 )
	(:div :id "menu_3" :class "dropmenudiv_b"
	      (:a :href "/show_properties" "Game properties")
	      (:a :href "/show_short_memory" "Shorthand memory")
	      (:a :href "/show_memory" "Rules")

	  )

	"<script type='text/javascript'>tabdropdown.init('bluemenu')</script>"
		 ))

    

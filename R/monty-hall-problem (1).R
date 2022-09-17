#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#' Contestant Selects a Door
#'
#' @description
#' The contestant is given the option to choose between three doors.
#'
#' @details
#' The select door function consists of the doors and the contestant’s choice.
#' The doors for the game has to be created and identified as 1, 2, or 3. After
#' the doors are created, the contestant is able to choose which door they want.
#' Using the code “sample” allows the computer to randomly select which door is
#' selected for each game.
#'
#' @param … no arguments are used by the function.
#'
#' @return
#' The return for the function is the randomly selected door by the “contestant”.
#'
#' @examples
#' select_door ()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host Opens Goat Door
#'
#' @description
#' The game randomly selects one of the goat doors to open.
#'
#' @details
#' The goat door function consists of the game and the contestant’s door
#' selection. The door selected by the contestant can not be chosen as one of
#' the doors so it must be accounted for in the function. As a result, the
#' contestant’s choice will affect the remaining doors. If the contestant pick
#' has a car behind the door than the two remaining doors have a goat behind
#' them; therefore, it does not matter which door is selected and a sample can
#' be chosen. If the contestant chooses a goat door, there will be both a car
#' and goat behind the remaining doors. The game as to make sure to not pick
#' door with the car behind it and to select the door with the goat behind it.
#'
#' @param … if the car door is selected then a sample size of 1 can be taken
#' from the remaining doors. If the goat door is selected, then the game has to
#' eliminate the door with the car behind it as an option. The remaining door to
#' be chosen will be a goat door.
#'
#' @return The return should be the selected goat door by the game.
#'
#' @examples
#' If (game[a.pick] = “goat”)
#'	  opened.door <- function(game, a.pick)
#'
#' @export
opened.door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change Doors
#'
#' @description
#' The contestant has the opportunity to keep the door they originally picked or
#' they can switch to the other door.
#'
#' @details
#' The function consists of whether they chose to stay the opened goat door and
#' the original choice. If they decide to stay than the stay option would be
#' true which means that the pick and the opened goat door would remain the same.
#' The final pick would be the originally selected door. If the contestant
#' choses to switch doors than the opened goat door would remain the same, but
#' the contestant’s choice would change to the other remaining door; therefore,
#' the opened goat door and the original choice would have to be ignored as a
#' choice.
#'
#' @param … if the contestant stays then there is no changes that need to occur
#' and all choices would be True. If the contestant decides to switch choices,
#' the final pick would have to eliminate the original choice and the opened
#' goat door by using the (!).
#'
#' @return
#' The final pick based on the contestant’s choice to stay or switch.
#'
#' @examples
#' final.pick <- doors[doors != opened.door & doors != a.pick]
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine if Contestant has Won
#'
#' @description
#' If the contestant picked the car door, they win the game.
#'
#' @details
#' The winner of the game is determined by whether the contestant chose the
#' correct door. If the final pick from the contestant is the car door, then
#' they have won the game. If the contestant’s final pick resulted with a goat
#' behind the door, then they lost the game and will receive the message “LOSE”
#' as a result.
#'
#' @param : … choosing a door with a goat behind it will result in a lost for
#' the contestant. If there is a car behind the chosen door than the contestant
#' has won the game.
#'
#' @return The result should either be a “Win” or a “Lose”.
#'
#' @examples determine_winner(final.pick) = 1, game = this.game)
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}



#' @title
#' Test the Game
#'
#' @description
#' The game must be tested to ensure that it works.
#'
#' @details
#' The commands must work together as in a single play. The commands that need
#' to be tested are the commands that generate results. Each result must rely on
#' each other and cannot produce conflicting results. The first step that need
#' to be tested is the game setup which is the location of the goats and the car.
#' The initial pick of the contestant, the opened goat door, and the final pick
#' must work together. For example, the contestant’s door cannot be the same as
#' the goat door. The final result has to be either a “Win” or a “Lose”. The
#' “Win” should not appear if the final door chosen is a goat door.
#'
#' @param : … The commands that are chosen are a result of the contestant
#' decides to either “Stay” or “Switch”.
#'
#' @return
#' The result should be 4 separate answers that shows whether the choices made
#' wins or loses.
#'
#' @examples
#'   this.game <- c("goat", "car", "goat")
#'       determine_winner (final.pick = 1, game = this.game)
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}



#' @title
#' Counting the odds
#'
#' @description
#' After the game is created, it needs to be tested multiple times to determine
#' if the odds increase if the contestant stays or if they switch doors.
#'
#' @details
#' The function created for this test needs to be tested over 100 times. The
#' test must be run on a loop for the count of 100.
#'
#' @param : … no arguments are used by the function.
#'
#' @return The result is that there will be 100 samples of results from the
#' game being played repeatedly.
#'
#' @examples
#' i.	For( i in 1:n)
#'	{
#'  	game.outcome <- play_game()
#'  	Results.list[[loop.count]] <- game.outcome
#'  	Loop.count <- loop.count + 1
#'  	}
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}

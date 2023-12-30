package o1.adventure

import scala.collection.mutable.Buffer

/** The class `Adventure` represents text adventure games. An adventure consists of a player and
  * a number of areas that make up the game world. It provides methods for playing the game one
  * turn at a time and for checking the state of the game.
  *
  * N.B. This version of the class has a lot of “hard-coded” information that pertains to a very
  * specific adventure game that involves a small trip through a twisted forest. All newly created
  * instances of class `Adventure` are identical to each other. To create other kinds of adventure
  * games, you will need to modify or replace the source code of this class. */
class World:

  /** the name of the game */
  val title = "Attack on Titan: Battle of Shiganshina"

  var trostDistrict = Area("Trost District", "You are in Trost District. It is our hometown, for now.", true)
  var diningHall    = Area("Dining hall", "You are in the dining hall. You just had the best feast of your life, in preparation for the toughest expedition, which begins tomorrow.", true)
  var erensRoom     = Area("Eren's room", "You are in your room. You should sleep for tomorrow.", false)
  var wallRose      = Area("Wall Rose", "You are standing atop Wall Rose; the last line of defence between humanity and the titans. Commander Erwin just gave the Scout's farewell speech, which was met with much fanfare.\nMount your horse and ride with the scouts into the titan forest.", false)
  var titanForest   = Area("Titan forest", "You've reached the titan forest and unmounted your horse, as you must move quietly. It is completely dark, since it is the night of the full moon.\nThe titans are dormant. Don't wake them up. Mount your horse once you've past them.", false)
  var innerWallMaria= Area("Wall Maria (inner)", "You stand atop Wall Maria, overlooking your home town (Shiganshina): the town where everything began.", false)
  var shiganshina   = Area("Shiganshina District", "The town where everything began. Danger lurks.", false)
  var outerWallMaria= Area("Wall Maria (outer)", "You stand atop Wall Maria, overlooking the giant field, completely devoid of all life besides from titans.", false)
  var erensHouse    = Area("Eren's house", "You are standing at your old home, though the only difference is that it is completely reduced to rubble.\n\nLuckily the basement was untouched. Use the key to go inside.", false)
  var basement      = Area("The basement", "The room where the truth lies. Search for it.", false)
  var giantField    = Area("Giant field", "There is nothing for miles, though the flowers are now budding, and the butterflies dancing. You see a stationary, harmless pure titan, though there is no reason to kill it. Mount your horse and ride to the sea!", false)
  var sea           = Area("The sea", "On the other side of the sea... is freedom. At least that is what I believed... but I was wrong.", false)

  trostDistrict .setNeighbors(Vector("west" -> diningHall, "east" -> erensRoom, "south" -> wallRose            ))
  diningHall    .setNeighbors(Vector("east" -> trostDistrict                                                   ))
  erensRoom     .setNeighbors(Vector("west" -> trostDistrict                                                   ))
  wallRose      .setNeighbors(Vector("south" -> titanForest, "north" -> trostDistrict                          ))
  titanForest   .setNeighbors(Vector("south" -> innerWallMaria, "north" -> wallRose                            ))
  innerWallMaria.setNeighbors(Vector("down" -> shiganshina, "south" -> outerWallMaria, "north" -> titanForest  ))
  shiganshina   .setNeighbors(Vector("east" -> erensHouse, "north" -> innerWallMaria, "south" -> outerWallMaria))
  outerWallMaria.setNeighbors(Vector("down" -> shiganshina, "north" -> innerWallMaria, "south" -> giantField   ))
  erensHouse    .setNeighbors(Vector("west" -> shiganshina, "down" -> basement                                 ))
  basement      .setNeighbors(Vector("up" -> erensHouse                                                        ))
  giantField    .setNeighbors(Vector("north" -> outerWallMaria, "south" -> sea                                 ))
  sea           .setNeighbors(Vector("north" -> giantField                                                     ))


  this.erensRoom.addItem(Item("basement key", "The key for the basement of Eren's home."))
  this.erensRoom.addItem(Item("ODM gear", "Omni-directional mobility gear: A type of equipment developed by humans that allows great mobility when facing the Titans in combat."))
  this.titanForest.addTitan(Titan("dormant titan", "This pure titan is dormant, as there is currently zero sunlight."))
  this.giantField.addTitan(Titan("stationary titan", "This pure titan is unable to move. There is no reason to attack it, as it is a fellow patriot."))

  /** The character that the player controls in the game. */
  var player = Player(trostDistrict)

  private var previousLocations = Buffer[Area](trostDistrict)
  
  def pastLocations = previousLocations
  
  def append() =
    this.previousLocations = this.previousLocations.appended(this.player.location)

  if this.player.location == this.diningHall then
    trostDistrict.setNeighbors(Vector("west" -> diningHall, "east" -> erensRoom))


  /** Determines if the adventure is complete, that is, if the player has won. */
  def isComplete = this.player.location == this.sea && this.player.inventory.contains("Green book") && this.player.inventory.contains("Black book") && this.player.inventory.contains("Red book")

  /** Determines whether the player has won, lost, or quit, thereby ending the game. */
  def isOver = this.isComplete || this.player.hasQuit || !this.player.isBroAlive

  /** Returns a message that is to be displayed to the player at the beginning of the game. */
  def welcomeMessage =
    "\n" +
    "WARNING: this game contains spoilers from Attack On Titan Season 3 Part 2. It is recommended that the TA assessing the game has watched it, as the precise decisions made by the characters in the anime were critical for the ending. \n\n" +
    "You are Eren Yeager, a member of the scout regiment and the main reason that humanity has endured and progressed this far. \n" +
    "The operation to retake Wall Maria commences tomorrow and there are only two possible outcomes: \n" +
    "Humanity prevails and reclaims all the land lost from five years ago by sealing off Wall Maria, or the operation fails, resulting in all of humanity being overrun by titans. \n" +
    "In addition, the truth about the origins of titans and humans lies in the basement of your old home. Reach it, and uncover the secrets of the titans and the world outside the walls. \n" +
    "For now though, a feast is taking place in the dining hall to celebrate the progress made by humanity in the past four months. Go, and enjoy."


  /** Returns a message that is to be displayed to the player at the end of the game. The message
    * will be different depending on whether or not the player has completed their quest. */
  def goodbyeMessage =
    if this.isComplete then
      "On the other side of the sea... is freedom. At least that is what I believed... but I was wrong.\n\nHey... if we kill all of our enemies over there... will we finally... be free?"
    else
      "You died! All of humanity was eaten."


  /** Plays a turn by executing the given in-game command, such as “go west”. Returns a textual
    * report of what happened, or an error message if the command was unknown. In the latter
    * case, no turns elapse. */
  def playTurn(command: String) =
    val action = Action(command)
    val outcomeReport = action.execute(this.player)
    outcomeReport.getOrElse(s"""Unknown command: "$command".""")

end World

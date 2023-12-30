package o1.adventure

import scala.collection.mutable.Map
import scala.collection.mutable.Buffer

/** A `Player` object represents a player character controlled by the real-life user
  * of the program.
  *
  * A player object’s state is mutable: the player’s location and possessions can change,
  * for instance.
  *
  * @param startingArea  the player’s initial location */
class Player(startingArea: Area):

  private var currentLocation = startingArea        // gatherer: changes in relation to the previous location
  private var quitCommandGiven = false              // one-way flag
  private var items = Map[String, Item]()
  private var titanTransformations = 0
  private var titan = false
  private var horizontalArrow = " <----> "
  private var verticalArrow = "^\n" + "                         |\n" * 3 + "                         ˇ"
  private var horse = false
  private var isAlive = true
  private var odm = false
  private var hard = false
  private var saved = ""

  def stuff = this.items

  /** Determines if the player has indicated a desire to quit the game. */
  def hasQuit = this.quitCommandGiven

  /** Returns the player’s current location. */
  def location = this.currentLocation

  def charges = this.titanTransformations

  def help =
    "\nCommands: " +
      "\n'go' followed by the direction of the next area to move to the next area. You may need to use ODM gear to travel between certain areas." +
      "\n'quit' to quit the game." +
      "\n'inventory' to see the items that you currently hold." +
      "\n'get' followed by the name of any item in your current area to pick up the item." +
      "\n'drop' followed by the name of any item in your inventory to drop the item." +
      "\n'examine' followed by the name of any item in you inventory or titan in the area to get a description of that item/titan." +
      "\n'sleep' to sleep. You can only sleep in your room. Sleeping refills your titan transformations to three." +
      "\n'transform' to transform into the Attack Titan. You have a maximum of three titan transformations." +
      "\n'harden' to use hardening while transformed." +
      "\n'map' to see the world map. Parts of the map unlock as you progress through the story." +
      "\n'mount' to mount your horse." +
      "\n'attack' followed by the name of any titan in your current area to attack the titan." +
      "\n'use' followed by the name of any item in your inventory to use it." +
      "\n'unequip' followed 'ODM gear' to unequip it." +
      "\n'save' followed by the name of the character you want to save." +
      "\n\nProgress through the story by using your titan powers to defeat the enemy."

  def unalive() =
    this.isAlive = false

  def isBroAlive = isAlive

  def isOdm = this.odm

  def isMounted = this.horse

  def isTitan = this.titan

  def isHardened = this.hard

  def characterSaved = this.saved

  /** Attempts to move the player in the given direction. This is successful if there
    * is an exit from the player’s current location towards the direction name. Returns
    * a description of the result: "You go DIRECTION." or "You can't go DIRECTION." */
  def go(direction: String) =
    val destination = this.location.neighbor(direction)
    if destination.isDefined && destination.get.availability then
      this.currentLocation = destination.getOrElse(this.currentLocation)
      "You go " + direction + "."
    else
      "You can't go " + direction + "."

  def get(itemName: String): String =
    if itemName == "Locked desk drawer" then
      "You can't take the desk drawer. Use something to open it!"
    else if this.location.contains(itemName) then
      var item = this.location.removeItem(itemName)
      this.items += item.get.name -> item.get
      s"You pick up the $itemName."
    else
      s"There is no $itemName here to pick up."

  def use(itemName: String): String =
    if this.inventory.contains(itemName) then

      if itemName == "ODM gear" then
        this.useOdm

      else if itemName == "basement key" then
        if this.location.name == "Eren's house" then
          this.location.neighbor("down").get.makeAvailable()
          this.location.description = "You are standing at your old home, though the only difference is that it is completely reduced to rubble.\n\nLuckily the basement was untouched. Go inside."
          "The key doesn't match the lock for the basement! Regardless, Levi kicks the door open and you go in anyway."
        else if this.location.name == "The basement" && this.location.contains("Locked desk drawer") then
          this.location.removeItem("Locked desk drawer")
          this.location.addItem(Item("Green book", "The Early Life of Grisha Yeager"))
          this.location.addItem(Item("Red book", "Information About the World Beyond the Walls"))
          this.location.addItem(Item("Black book", "The Extent of Our Knowledge of Titans and Their History"))
          this.location.description = "The room where the truth lies."
          "You opened the desk drawer and found three books. Take them, and examine if you wish."
        else
          "You can't use the key here!"

      else
        "You can only examine books!"
    else
      "You don't have that!"

  def useOdm: String =
    if (this.location.toString.contains("Wall Maria (inner)") || this.location.toString.contains("Titan forest") || this.location.toString.contains("Wall Maria (outer)") || this.location.toString.contains("Shiganshina District") || this.location.name == "Giant field") && this.inventory.contains("ODM gear") && !this.isTitan && !this.isOdm then
      this.odm = true
      if this.isMounted then
        this.horse = false
        "ODM gear equipped. Horse unmounted."
      else
        "ODM gear equipped."
    else if this.odm then
      "ODM gear already equipped."
    else if this.isTitan then
      "You're a titan!"
    else
      "You can't use ODM gear here."

  def unequip(itemName: String): String =
    if this.inventory.contains(itemName) then
      if itemName == "ODM gear" && this.odm then
        this.odm = false
        "You unequipped the ODM gear."
      else
        "You're not using it."
    else
      "You don't have that!"


  def attack(titanName: String): String =
    if this.location.containsTitans(titanName) && !this.location.getTitan(titanName).get.isDefeated then

      if titanName != "Armoured titan" && titanName != "Colossal titan" then

        this.location.getTitan(titanName).get.defeat()
        s"You slew the $titanName."

      else if titanName == "Armoured titan" then
        if this.isTitan && this.isHardened then
          this.location.getTitan(titanName).get.defeat()
          this.unharden()
          "You defeated the Armoured titan!"
        else if !this.isTitan then
          "You weren't transformed!"
        else
          "You didn't use hardening!"

      else
        if this.isTitan && this.location.name == "Shiganshina District" && this.location.description.contains("Suddenly, the Armoured Titan") then
          this.location.neighbor("north").get.makeAvailable()
          this.go("north")
          "The Colossal Titan kicked you onto the wall."
        else if this.isTitan && this.location.name == "Shiganshina District" && this.location.description == "Seal the wall by hardening and attack the Colossal Titan using ODM gear!" then
          "Your titan body is paralyzed!"
        else if this.isTitan && this.location.name == "Shiganshina District" && this.location.description == "You successfully plugged the wall! Go and save Armin!" then
          "You, along with Armin got burnt alive."
        else
          if this.isOdm then
            this.location.getTitan("Colossal titan").get.defeat()
            "You caught Bertholdt lacking. With a long slash of your blades, you pull Bertholdt out of the Colossal Titan's nape."
          else
            "You forgot to use ODM gear, resulting in you getting cooked alive."

    else if this.location.titansList.contains(titanName) && this.location.getTitan(titanName).get.isDefeated then
      s"$titanName is already defeated!"
    else
      s"There is no $titanName here to slay."

  def save(character: String):String =
    if character == "Armin" then
      this.saved = "Armin"
      "You chose to inject Armin. The Colossal Titan has a new inheritor."
    else if character == "Erwin" then
      this.saved = "Erwin"
      "You chose to inject Erwin. The Colossal Titan has a new inheritor."
    else
      s"$character isn't savable."

  def drop(itemName: String): String =
    if this.items.contains(itemName) then
      val item = this.items(itemName)
      this.items.remove(itemName)
      this.location.addItem(item)
      s"You drop the $itemName."
    else
      "You don't have that!"

  def examine(thingName: String): String =
    if this.items.contains(thingName) then
      s"You look closely at the $thingName.\n${this.items(thingName).description}"
    else if this.location.titansList.contains(thingName) then
      s"You look at the $thingName. \n${this.location.titansList(thingName).description}"
    else
      "If you want to examine an item, then pick it up first. Otherwise if you want to examine a titan, it needs to be in the area."

  def has(itemName: String): Boolean =
    this.items.contains(itemName)

  def inventory: String =
    if this.items.isEmpty then
      "You are empty-handed."
    else
      "You are carrying:\n" + this.items.keys.mkString("\n")

  def sleep: String =
    if this.location.name == "Eren's room" then
      this.titanTransformations = 3
      "You feel well rested."
    else
      "You can't sleep here!"

  def transform: String =
    if this.titanTransformations > 0 && !this.titan && ((this.location.name == "Wall Maria (outer)" && this.location.neighbor("down").get.titansList.isEmpty) || this.location.name == "Shiganshina District") && !(this.saved == "Armin"|| this.saved == "Erwin") then //change so that it's only shiganshina later
      this.titan = true
      this.odm = false
      this.horse = false
      this.titanTransformations -= 1
      "You bite your hand and transform into the Attack Titan."
    else if this.isTitan then
      "You're already transformed!"
    else if !(this.location.name == "Wall Maria (Outer)" || this.location.name == "Shiganshina District") then
      "You can't transform here!"
    else if this.saved == "Armin" || this.saved == "Erwin" then
      "You can't transform."
    else
      "No more titan transformations left."

  def untransform =
    if this.isTitan then
      this.titan = false
      this.hard = false
      "You are human again."
    else
      "You're already human."

  def harden =
    if this.isTitan then
      this.hard = true
      "You used hardening."
    else
      "Transform first!"

  def unharden() =
    if this.isHardened then
      this.hard = false

  def mount: String =
    if !this.horse && (this.location.name == "Wall Rose" || this.location.name == "Titan forest" || this.location.name == "Giant field")  then
      if this.odm then
        this.horse = true
        this.unequip("ODM gear").dropRight(1) + " and mounted your horse."
      else
        this.horse = true
        "You mounted your horse."
    else if this.horse then
      "You're already mounted."
    else
      "You can't mount here."

  def unmount: String =
    if this.horse then
      this.horse = false
      "You unmounted your horse."
    else
      "You're already unmounted."

  def map: String =

    "Dining Hall" + horizontalArrow + "Trost District" + horizontalArrow + "Eren's room\n" +
    "                         " + verticalArrow + "                         \n" +
    "                     " + "Wall Rose\n" +
    "                         " + verticalArrow + "                         \n" +
    "                    " + "Titan Forest\n" +
    "                         " + verticalArrow + "                         \n" +
    "                 " + "Wall Maria (inner)" + "\n" +
    "                         " + verticalArrow + "                         \n" +
    "                " + "Shiganshina District" + horizontalArrow + "Eren's House" + horizontalArrow + "The Basement\n" +
    "                         " + verticalArrow + "                         \n" +
    "                 " + "Wall Maria (outer)" + "\n" +
    "                         " + verticalArrow + "                         \n" +
    "                    " + "Giant Field\n" +
    "                         " + verticalArrow + "                         \n" +
    "                      " + "The Sea\n"




  /** Signals that the player wants to quit the game. Returns a description of what happened within
    * the game as a result (which is the empty string, in this case). */
  def quit() =
    this.quitCommandGiven = true
    ""

  /** Returns a brief description of the player’s state, for debugging purposes. */
  override def toString = "Now at: " + this.location.name

end Player

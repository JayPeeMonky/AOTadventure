package o1.adventure.ui

import o1.adventure.*
import o1.adventure.ui.AdventureTextUI.game

import scala.collection.mutable.Buffer
import scala.io.StdIn.*

/** The singleton object `AdventureTextUI` represents a fully text-based version of the
  * Adventure game application. The object serves as an entry point for the game, and
  * it can be run to start up a user interface that operates in the text console.
  * @see [[AdventureGUI]] */
object AdventureTextUI extends App:

  private val game = World()
  private val player = game.player
  this.run()

  /** Runs the game. First, a welcome message is printed, then the player gets the chance to
    * play any number of turns until the game is over, and finally a goodbye message is printed. */
  private def run() =
    println(this.game.welcomeMessage)
    while !this.game.isOver do
      this.printAreaInfo()
      this.playTurn()

      game.append()

      if game.pastLocations.contains(game.diningHall) then
        game.erensRoom.makeAvailable()
        player.map

      if game.pastLocations.count( _ == game.diningHall ) > 1 then
        game.diningHall.description = "Get the key from your room"

      if player.charges == 3 then
        game.erensRoom.description = "You feel well rested. Get prepared for the operation, then go to Wall Rose for your farewell."

      if player.inventory.contains("basement key") && this.player.inventory.contains("ODM gear") && this.player.charges == 3 && player.location == game.erensRoom then
        game.erensRoom.description = "You are now prepared. Go to Wall Rose for your farewell."
        game.wallRose.makeAvailable()


      if game.pastLocations.count( _ == game.wallRose ) > 1 && player.isMounted then
        game.wallRose.description = "You are standing atop Wall Rose; the last line of defence between humanity and the titans. Go into the titan forest."
        game.titanForest.makeAvailable()

      if player.location == game.wallRose && !player.isMounted && game.pastLocations.count( _ == game.wallRose) > 1 then
        game.titanForest.makeUnavailable()
        game.wallRose.description = "Mount your horse and ride with the scouts into the titan forest."

      if player.location != game.wallRose && player.location != game.titanForest && player.location != game.giantField then
        player.unmount

      if game.titanForest.getTitan("dormant titan").get.isDefeated then
        println("You woke the titan up, causing it to alert every other titan in the area and consume the entire scout regiment.")
        this.player.unalive()

      if game.pastLocations.count( _ == game.titanForest ) == 1 then
        player.unmount

      if player.location == game.titanForest && (player.isMounted || player.isOdm) then
        game.wallRose.makeUnavailable()

      if game.pastLocations.count( _ == game.titanForest ) >= 1 && !game.titanForest.getTitan("dormant titan").get.isDefeated && player.isMounted then
        game.titanForest.description = "You and the scouts ride until sunrise, finally reaching Wall Maria's inner gate. Commander Erwin instructs you to seal the inner and outer gate, the latter in which you do first. Switch to ODM gear to get onto the wall, then swing towards the outer gate."

      if game.pastLocations.count( _ == game.titanForest ) >= 1 && !game.titanForest.getTitan("dormant titan").get.isDefeated && player.isOdm && game.titanForest.description == "You and the scouts ride until sunrise, finally reaching Wall Maria's inner gate. Commander Erwin instructs you to seal the inner and outer gate, the latter in which you do first. Switch to ODM gear to get onto the wall, then swing towards the outer gate." then
        game.innerWallMaria.makeAvailable()
        game.outerWallMaria.makeAvailable()
        game.innerWallMaria.description = "You stand atop Wall Maria, overlooking your home town (Shiganshina): the town where everything began.\nSeal the outer gate."
        game.outerWallMaria.description = "You stand atop Wall Maria, overlooking the giant field, completely devoid of all life besides from titans.\nSeal the outer gate by first transforming, then hardening."
        game.titanForest.description = "Go to the outer gate and seal it by hardening."

      if !player.isOdm && player.location == game.titanForest && (game.titanForest.description == "Go to the outer gate and seal it by hardening." || game.titanForest.description == "You and the scouts ride until sunrise, finally reaching Wall Maria's inner gate. Commander Erwin instructs you to seal the inner and outer gate, the latter in which you do first. Switch to ODM gear to get onto the wall, then swing towards the outer gate.") then
        game.innerWallMaria.makeUnavailable()
        game.outerWallMaria.makeUnavailable()
        game.titanForest.description = "You and the scouts ride until sunrise, finally reaching Wall Maria's inner gate. Commander Erwin instructs you to seal the inner and outer gate, the latter in which you do first. Switch to ODM gear to get onto the wall, then swing towards the outer gate."

      if !player.isOdm && player.location == game.innerWallMaria then
        game.outerWallMaria.makeUnavailable()
        game.titanForest.makeUnavailable()
        game.shiganshina.makeUnavailable()

      if player.isOdm && player.location == game.innerWallMaria && game.shiganshina.titansList.isEmpty then
        game.outerWallMaria.makeAvailable()
        game.titanForest.makeAvailable()

      if player.isOdm && player.location == game.innerWallMaria && game.shiganshina.titansList.nonEmpty then
        game.shiganshina.makeAvailable()
        game.outerWallMaria.makeAvailable()

      if !player.isOdm && player.location == game.outerWallMaria then
        game.shiganshina.makeUnavailable()
        game.giantField.makeUnavailable()
        game.innerWallMaria.makeUnavailable()

      if player.isOdm && player.location == game.outerWallMaria && game.shiganshina.titansList.isEmpty then
        game.innerWallMaria.makeAvailable()

      if !player.isOdm && player.location == game.outerWallMaria && game.shiganshina.titansList.isEmpty then
        game.innerWallMaria.makeUnavailable()

      if player.isOdm && player.location == game.outerWallMaria && game.shiganshina.titansList.nonEmpty then
        game.innerWallMaria.makeAvailable()

      if player.isOdm && player.location == game.outerWallMaria && game.shiganshina.titansList.nonEmpty && (player.characterSaved == "Armin" || player.characterSaved == "Erwin") then
        game.innerWallMaria.makeAvailable()
        game.giantField.makeAvailable()

      if (player.isTitan || !player.isOdm) && player.location == game.shiganshina then
        game.innerWallMaria.makeUnavailable()
        game.outerWallMaria.makeUnavailable()

      if player.isOdm && player.location == game.shiganshina && !game.shiganshina.titansList.contains("Colossal titan") then
        game.innerWallMaria.makeAvailable()
        game.outerWallMaria.makeAvailable()

      if game.shiganshina.titansList.contains("Armoured titan") then
        game.innerWallMaria.description = "You stand atop Wall Maria, overlooking your home town (Shiganshina): the town where everything began."

      if player.location == game.outerWallMaria && player.isTitan && player.isHardened then
        game.outerWallMaria.description = "You successfully sealed the outer gate... was it really that simple? Mikasa pulls your body out of your titan body."
        player.unharden()
        player.untransform
        game.outerWallMaria.description += "\n." * 5
        game.outerWallMaria.description += "\nA scout finds Reiner from within the wall! Levi fails to finish him off, causing him to transform.\n\nBOOM\n\nThe beast titan along with dozens of pure titans appear from flashes of lightning.\n\nUse your ODM gear to get down to Shiganshina District, transform, use hardening, and Engage with Reiner!"
        game.shiganshina.addTitan(Titan("Armoured titan", "One of the Nine Titans that possesses armored plates of skin across its body."))
        game.titanForest.makeUnavailable()


      if player.location == game.outerWallMaria && player.isOdm && game.shiganshina.titansList.nonEmpty then
        game.shiganshina.makeAvailable()


      if player.location == game.shiganshina && game.shiganshina.getTitan("Armoured titan").get.isDefeated && !game.shiganshina.titansList.contains("Colossal titan") then
        game.shiganshina.description = "Suddenly, the Armoured Titan lets out a deafening scream. The beast titan throws a barrel into Shiganshina. Armin immediately figures that it's Bertholdt inside, thus you all run away. Bertholdt transforms, blowing up most of Shiganshina. Attack the Colossal Titan!"
        game.shiganshina.addTitan(Titan("Colossal titan", "This Titan is notable for its massive size and significant control over the steam emitted by its Titan body along with the user's control over the power of the blast released by its transformation."))
        game.innerWallMaria.makeUnavailable()
        game.outerWallMaria.makeUnavailable()

      if player.isTitan && player.location == game.innerWallMaria then
        player.unharden()
        game.shiganshina.makeAvailable()
        game.innerWallMaria.description = "Armin comes to you and shares his plan. The plan is for you to distract the Colossal Titan by falling and sealing the inner gate by hardening.\n\nThe Colossal Titan is approaching! Fall down to Shiganshina and harden to seal the inner gate!"

      if player.isTitan && player.location == game.innerWallMaria && game.shiganshina.description == "Suddenly, the Armoured Titan lets out a deafening scream. The beast titan throws a barrel into Shiganshina. Armin immediately figures that it's Bertholdt inside, thus you all run away. Bertholdt transforms, blowing up most of Shiganshina. Attack the Colossal Titan!" then
        player.unharden()
        game.shiganshina.description = "Seal the wall by hardening and attack the Colossal Titan using ODM gear!"

      if player.isTitan && player.isHardened && player.location == game.shiganshina && game.shiganshina.getTitan("Armoured titan").get.isDefeated && !game.shiganshina.getTitan("Colossal titan").get.isDefeated && game.shiganshina.description == "Seal the wall by hardening and attack the Colossal Titan using ODM gear!" then
        player.untransform
        game.innerWallMaria.makeUnavailable()
        game.outerWallMaria.makeUnavailable()
        game.shiganshina.description = "You successfully plugged the wall! Go and save Armin!"

      if player.location == game.shiganshina && game.shiganshina.getTitan("Armoured titan").get.isDefeated && game.shiganshina.getTitan("Colossal titan").get.isDefeated then
        game.innerWallMaria.makeUnavailable()
        game.outerWallMaria.makeUnavailable()
        game.shiganshina.description = "The battle is finally over. Armin is burnt to a crisp. You want to save him by injecting him with the titan serum however, stupid idiot Flock brings back Erwin, who is also on the brink of death. Who do you choose to save?"

      if player.location == game.shiganshina && game.shiganshina.getTitan("Armoured titan").get.isDefeated && game.shiganshina.getTitan("Colossal titan").get.isDefeated && (player.characterSaved == "Armin" || player.characterSaved == "Erwin") then
        game.erensHouse.makeAvailable()
        game.shiganshina.description = "Go to the basement with Mikasa, Levi, and Hange."
        game.basement.addItem(Item("Locked desk drawer", "A locked desk drawer."))

      if player.inventory.contains("Green book") || player.inventory.contains("Black book") || player.inventory.contains("Red book") then
        game.giantField.makeAvailable()
        game.outerWallMaria.makeAvailable()
        game.shiganshina.makeAvailable()
        game.titanForest.makeAvailable()
        game.wallRose.makeAvailable()
        game.diningHall.description = "You are in the dining hall."
        game.erensRoom.description = "You are in your room."
        game.wallRose.description = "You are standing atop Wall Rose."
        game.titanForest.description = "Just an ordinary forest."
        game.innerWallMaria.description = "You stand atop Wall Maria, overlooking your home town (Shiganshina): the town where everything began."
        game.shiganshina.description = "The town where everything began."
        game.erensHouse.description = "You are standing at your old home, though the only difference is that it is completely reduced to rubble."
        game.basement.description = "The room where the truth lies.\n\nYou can now explore the entire game world. Go to the sea with all three books to complete the game."
        game.outerWallMaria.description = "You stand atop Wall Maria, overlooking the giant field."


        if player.location == game.shiganshina && player.isOdm then
          game.innerWallMaria.makeAvailable()
          game.outerWallMaria.makeAvailable()

        if player.location == game.shiganshina && !player.isOdm then
          game.outerWallMaria.makeUnavailable()
          game.innerWallMaria.makeUnavailable()

        if player.location == game.outerWallMaria && player.isOdm then
          game.giantField.makeAvailable()
          game.shiganshina.makeAvailable()
          game.innerWallMaria.makeAvailable()

        if player.location == game.outerWallMaria && !player.isOdm then
          game.giantField.makeUnavailable()
          game.shiganshina.makeUnavailable()
          game.innerWallMaria.makeUnavailable()

        if player.location == game.innerWallMaria && player.isOdm then
          game.shiganshina.makeAvailable()
          game.outerWallMaria.makeAvailable()
          game.titanForest.makeAvailable()

        if player.location == game.innerWallMaria && !player.isOdm then
          game.shiganshina.makeUnavailable()
          game.outerWallMaria.makeUnavailable()
          game.titanForest.makeUnavailable()

        if player.location == game.titanForest && player.isOdm then
          game.innerWallMaria.makeAvailable()

        if player.location == game.titanForest && !player.isOdm then
          game.innerWallMaria.makeUnavailable()

      if player.location == game.giantField && player.isOdm then
        game.outerWallMaria.makeAvailable()

      if player.location == game.giantField && !player.isOdm then
        game.outerWallMaria.makeUnavailable()

      if player.location == game.sea && (player.inventory.contains("Green book") && player.inventory.contains("Black book") && player.inventory.contains("Red book")) then
        game.sea.description += "\n\nYou're missing the book(s). If you wish to complete the game, you must have all three of the books from the basement!"

      if player.location == game.giantField && player.isMounted then
        game.sea.makeAvailable()

      if player.location == game.giantField && !player.isMounted then
        game.sea.makeUnavailable()


    println("\n" + this.game.goodbyeMessage)


  /** Prints out a description of the player character’s current location, as seen by the character. */
  private def printAreaInfo() =
    val area = this.player.location
    println("\n\n" + area.name)
    println("-" * area.name.length)
    println(area.fullDescription + "\n")


  /** Requests a command from the player, plays a game turn accordingly, and prints out a
    * report of what happened.  */
  private def playTurn() =
    println()
    val command = readLine("Command: ")
    val turnReport = this.game.playTurn(command)
    if turnReport.!=("") then
      println(turnReport)

    if turnReport.contains("You weren't transformed!") then
      player.unalive()
    else if turnReport.contains("You didn't use hardening!") then
      player.unalive()
    else if turnReport.contains("You forgot to use ODM gear, resulting in you getting cooked alive.") then
      player.unalive()
    else if turnReport.contains("You, along with Armin got burnt alive.") then
      player.unalive()

end AdventureTextUI

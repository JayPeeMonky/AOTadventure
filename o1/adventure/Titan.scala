package o1.adventure

class Titan(val name: String, val description: String):

  private var defeated = false

  def isDefeated = this.defeated

  def defeat() =
    this.defeated = true

  override def toString = this.name

/**
 * Copyright 2012 David Stark
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package src.util

import java.awt.image.BufferedImage
import java.awt.Color

import src.runner.Main

import src.Civ

object CivSprite {

  val GOLD : Color = new Color(255, 255, 36)
  val COPPER : Color = new Color(202, 139, 70)
  val C_COIN : BufferedImage = MediaProvider.tint(MediaProvider.getImage("misc/money"), COPPER)
  val S_COIN : BufferedImage = MediaProvider.getImage("misc/money")
  val G_COIN : BufferedImage = MediaProvider.tint(MediaProvider.getImage("misc/money"), GOLD)

  val TECH : BufferedImage = MediaProvider.tint(MediaProvider.getImage("misc/science"), new Color(100, 120, 255, 127))
  val MIL_TECH : BufferedImage = MediaProvider.tint(MediaProvider.getImage("misc/science"), new Color(255, 100, 120, 127))

  val SCI_1 : Color = Color.GREEN
  val SCI_10 : Color = new Color(0, 255, 100)
  val SCI_100 : Color = new Color(0, 100, 255)
  val SCIENCE_100 : BufferedImage = MediaProvider.tint(MediaProvider.getImage("misc/science"), SCI_100)
  val SCIENCE_10 : BufferedImage = MediaProvider.tint(MediaProvider.getImage("misc/science"), SCI_10)
  val SCIENCE_1 : BufferedImage = MediaProvider.tint(MediaProvider.getImage("misc/science"), SCI_1)

  val SHIP_100 : BufferedImage = MediaProvider.scale(MediaProvider.getImage("misc/ship"), 24)
  val SHIP_10 : BufferedImage = MediaProvider.scale(MediaProvider.getImage("misc/ship"), 20)
  val SHIP_1 : BufferedImage = MediaProvider.getImage("misc/ship")

  val EXPEDITION : BufferedImage = MediaProvider.border(MediaProvider.getImage("misc/ship_large"))

}

import CivSprite._

class CivSprite(c : Civ, forShip : Boolean) extends Sprite(c.getSprite, if (forShip) 0 else 160 / 2 - 32 / 2, -32) {

  var resSprites : List[Sprite] = Nil
  var sciSprites : List[Sprite] = Nil
  var fleetSprites : List[Sprite] = Nil
  var techSprites : List[Sprite] = Nil
  var milTechSprites : List[Sprite] = Nil

  def init : Unit = {
    changeRes(0, c.resources)
    changeScience(0, c.science)
    changeFleet(0, c.military)
    changeTech(c.techLevel)
    changeMilTech(c.weapLevel)
  }

  def changeTech(to : Int) : Unit = {
    while (techSprites.size > to) {
      Main.add(Stage.remove(techSprites.last))
      techSprites = techSprites.init
    }
    while (techSprites.size < to) {
      val ts : Sprite = new Sprite(TECH, -techSprites.size * 4 - 16, 16)
      Main.add(Stage.add(ts, Some(this)))
      techSprites = techSprites ++ List(ts)
    }
    for (i <- 0 until milTechSprites.size) {
      Main.add(Stage.move(milTechSprites(i), -techSprites.size * 4 - i * 4 - 16, 16))
    }
  }

  def changeMilTech(to : Int) : Unit = {
    while (milTechSprites.size > to) {
      Main.add(Stage.remove(milTechSprites.last))
      milTechSprites = milTechSprites.init
    }
    while (milTechSprites.size < to) {
      val mts : Sprite = new Sprite(MIL_TECH, -techSprites.size * 4 - milTechSprites.size * 4 - 16, 16)
      Main.add(Stage.add(mts, Some(this)))
      milTechSprites = milTechSprites ++ List(mts)
    }
  }

  def changeScience(from : Int, to : Int) : Unit = {
    val oldGold : Int = from / 100
    val oldSilver : Int = (from % 100) / 10
    val oldCopper : Int = from % 10

    val newGold : Int = to / 100
    val newSilver : Int = (to % 100) / 10
    val newCopper : Int = to % 10
    val newTotal : Int = newGold + newSilver + newCopper
    val oldSize : Int = sciSprites.size
    if (oldSize > newTotal) {
      for (i <- newTotal until sciSprites.size) {
        Main.add(Stage.remove(sciSprites(i)))
      }
      while (sciSprites.size > newTotal) {
        sciSprites = sciSprites.init
      }
    }
    if (oldSize < newTotal) {
      for (i <- oldSize until newTotal) {
        val rs : Sprite = new Sprite(if (i >= newGold) if (i >= newSilver) SCIENCE_1 else SCIENCE_10 else SCIENCE_100, 32 + i * 4, 16)
        sciSprites = sciSprites ++ List(rs)
        Main.add(Stage.add(rs, Some(this)))
      }
    }

    for (i <- 0 until Math.min(oldSize, newTotal)) {
      val goldThen : Boolean = i < oldGold
      val goldNow : Boolean = i < newGold
      val silverThen : Boolean = i < oldSilver && i >= oldGold
      val silverNow : Boolean = i < newSilver && i >= newGold
      val copperThen : Boolean = !goldThen && !silverThen
      val copperNow : Boolean = !goldNow && !silverNow
      if (goldNow && !goldThen) {
        Main.add(Stage.change(sciSprites(i), SCIENCE_100))
      }
      if (silverNow && !silverThen) {
        Main.add(Stage.change(sciSprites(i), SCIENCE_10))
      }
      if (copperNow && !copperThen) {
        Main.add(Stage.change(sciSprites(i), SCIENCE_1))
      }
    }
  }

  def changeRes(from : Int, to : Int) : Unit = {
    val oldGold : Int = from / 100
    val oldSilver : Int = (from % 100) / 10
    val oldCopper : Int = from % 10

    val newGold : Int = to / 100
    val newSilver : Int = (to % 100) / 10
    val newCopper : Int = to % 10
    val newTotal : Int = newGold + newSilver + newCopper
    val oldSize : Int = resSprites.size
    if (oldSize > newTotal) {
      for (i <- newTotal until resSprites.size) {
        Main.add(Stage.remove(resSprites(i)))
      }
      while (resSprites.size > newTotal) {
        resSprites = resSprites.init
      }
    }
    if (oldSize < newTotal) {
      for (i <- oldSize until newTotal) {
        val rs : Sprite = new Sprite(if (i >= newGold) if (i >= newSilver) C_COIN else S_COIN else G_COIN, 32 + i * 4, 0)
        resSprites = resSprites ++ List(rs)
        Main.add(Stage.add(rs, Some(this)))
      }
    }

    for (i <- 0 until Math.min(oldSize, newTotal)) {
      val goldThen : Boolean = i < oldGold
      val goldNow : Boolean = i < newGold
      val silverThen : Boolean = i < oldSilver && i >= oldGold
      val silverNow : Boolean = i < newSilver && i >= newGold
      val copperThen : Boolean = !goldThen && !silverThen
      val copperNow : Boolean = !goldNow && !silverNow
      if (goldNow && !goldThen) {
        Main.add(Stage.change(resSprites(i), G_COIN))
      }
      if (silverNow && !silverThen) {
        Main.add(Stage.change(resSprites(i), S_COIN))
      }
      if (copperNow && !copperThen) {
        Main.add(Stage.change(resSprites(i), C_COIN))
      }
    }
  }

  def changeFleet(from : Int, to : Int) : Unit = {
    val oldGold : Int = from / 100
    val oldSilver : Int = (from % 100) / 10
    val oldCopper : Int = from % 10

    val newGold : Int = to / 100
    val newSilver : Int = (to % 100) / 10
    val newCopper : Int = to % 10
    val newTotal : Int = newGold + newSilver + newCopper
    val oldSize : Int = fleetSprites.size
    if (oldSize > newTotal) {
      for (i <- newTotal until fleetSprites.size) {
        Main.add(Stage.remove(fleetSprites(i)))
      }
      while (fleetSprites.size > newTotal) {
        fleetSprites = fleetSprites.init
      }
    }
    if (oldSize < newTotal) {
      for (i <- oldSize until newTotal) {
        val rs : Sprite =
          if (i >= newGold)
            if (i >= newSilver)
              new Sprite(SHIP_1, -4 - i * 4, 0)
            else new Sprite(SHIP_10, -4 - i * 4 - 4, -2)
          else new Sprite(SHIP_100, -4 - i * 4 - 8, -4)
        fleetSprites = fleetSprites ++ List(rs)
        Main.add(Stage.add(rs, Some(this)))
      }
    }

    for (i <- 0 until Math.min(oldSize, newTotal)) {
      val goldThen : Boolean = i < oldGold
      val goldNow : Boolean = i < newGold
      val silverThen : Boolean = i < oldSilver && i >= oldGold
      val silverNow : Boolean = i < newSilver && i >= newGold
      val copperThen : Boolean = !goldThen && !silverThen
      val copperNow : Boolean = !goldNow && !silverNow
      if (goldNow && !goldThen) {
        Main.add(Stage.change(fleetSprites(i), SHIP_100))
        Main.add(Stage.move(fleetSprites(i), -4 - i * 4 - 8, -4))
      }
      if (silverNow && !silverThen) {
        Main.add(Stage.change(fleetSprites(i), SHIP_10))
        Main.add(Stage.move(fleetSprites(i), -4 - i * 4 - 4, -2))
      }
      if (copperNow && !copperThen) {
        Main.add(Stage.change(fleetSprites(i), SHIP_1))
        Main.add(Stage.move(fleetSprites(i), -4 - i * 4, 0))
      }
    }
  }

}

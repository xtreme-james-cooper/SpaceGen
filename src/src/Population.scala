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

package src

import src.util.Stage
import src.runner.Main
import src.util.Sprite
import java.awt.image.BufferedImage

class Population(var typ : SentientType, var size : Int, val p : Planet) {

  var sprites : List[Sprite] = Nil //real

  if (size > 0) {
    Main.animate(Stage.tracking(p.sprite, Stage.delay(10)))
  }
  p.inhabitants = p.inhabitants ++ List(this)
  p.sprite.popSprites = p.sprite.popSprites + (this -> sprites)
  p.sprite.rearrangePopulation
  for (i <- 0 until size) {
    val s : Sprite = new Sprite(getSprite, p.sprite.popX(this, i), 0)
    sprites = sprites ++ List(s)
    Main.add(Stage.add(s, Some(p.sprite)))
  }
  Main.animate
  Main.animate(Stage.delay(10))

  override def toString : String =
    size + " billion " + (if (p.owner.isDefined && !p.owner.get.fullMembers.contains(typ)) "enslaved " else "") + typ.name

  def toUnenslavedString : String = size + " billion " + typ.name

  def increase(amt : Int) : Unit = {
    size = size + amt
    p.sprite.rearrangePopulation
    for (i <- 0 until amt) {
      val s : Sprite = new Sprite(getSprite, p.sprite.popX(this, size - amt + i), 0)
      sprites = sprites ++ List(s)
      Main.add(Stage.add(s, Some(p.sprite)))
    }
    Main.animate
  }

  def decrease(amt : Int) : Unit = {
    for (i <- 0 until amt) {
      val s : Sprite = sprites.last
      sprites = sprites.init
      Main.add(Stage.remove(s))
    }
    Main.animate
    size = size - amt
    p.sprite.rearrangePopulation
  }

  def eliminate : Unit = {
    Main.animate(Stage.tracking(p.sprite, Stage.delay(10)))
    for (s <- sprites) Main.add(Stage.remove(s))
    sprites = Nil
    size = 0 
    Main.animate
    p.inhabitants = p.inhabitants.filter(_ != this)
    p.sprite.popSprites = p.sprite.popSprites - this
    p.sprite.rearrangePopulation
    Main.animate(Stage.delay(10))
  }

  def send(target : Planet) : Unit = {
    Main.animate(Stage.tracking(p.sprite, Stage.delay(10)))
    val mover : Sprite = sprites.last
    sprites = sprites.init
    var targetPop : Population = getTargetPop(target)

    Main.animate(Stage.emancipate(mover))
    Main.animate(Stage.tracking(mover, Stage.move(mover, target.sprite.x + target.sprite.popX(targetPop, targetPop.size - 1), target.sprite.y)))
    Main.animate(Stage.subordinate(mover, target.sprite))
    target.sprite.popSprites = target.sprite.popSprites + (targetPop -> (target.sprite.popSprites.get(targetPop).get :+ mover)) //TODO get

    if (size == 1) {
      p.inhabitants = p.inhabitants.filter(_ != this)
      p.sprite.popSprites = p.sprite.popSprites - this
    } else {
      size = size - 1
    }

    p.sprite.rearrangePopulation
    addUpdateImgs
  }

  private def getTargetPop(target : Planet) : Population = {
    for {
      pop <- target.inhabitants
      if pop.typ == typ
    } {
      pop.setSize(pop.size + 1)
      target.sprite.rearrangePopulation
      return pop
    }

    val pop : Population = new Population(typ, 0, target)
    pop.setSize(1)
    target.sprite.rearrangePopulation
    pop
  }

  def update : Unit = {
    Main.animate(Stage.tracking(p.sprite, Stage.delay(10)))
    for (s <- sprites) Main.add(Stage.change(s, getSprite))
    Main.animate
  }

  def addUpdateImgs : Unit = for (s <- sprites) Main.add(Stage.change(s, getSprite))

  def setSize(newSize : Int) : Unit = {
    val diff : Int = newSize - size
    if (diff > 0)
      increase(diff)
    else if (diff < 0)
      decrease(-diff)
  }

  def getSprite : BufferedImage =
    typ.getSprite(false, null, p.owner.isDefined && !p.owner.get.fullMembers.contains(typ), p.owner.isEmpty)

}

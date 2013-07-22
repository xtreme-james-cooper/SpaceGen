package src.util

import java.awt.image.BufferedImage

sealed abstract class Animation {
  def tick(stage : Stage) : Option[Animation]
}

class Delay(waitTime : Int) extends Animation {

  def tick(stage : Stage) : Option[Animation] = if (waitTime > 0) Some(new Delay(waitTime - 1)) else None

}

class Tracking(s : Sprite, a : Option[Animation], tick : Int, var time : Int, var sx : Int, var sy : Int, lock : Boolean) extends Animation {

  def this(s : Sprite, a : Animation) = this(s, Some(a), 0, 0, 0, 0, false)
  def this(s : Sprite) = this(s, None, 0, 0, 0, 0, false)

  def tick(stage : Stage) : Option[Animation] = {
    if (stage.doTrack) {
      val tx : Int = s.globalX + s.img.getWidth / 2
      val ty : Int = s.globalY + s.img.getHeight / 2
      if (tick == 0) {
        sx = stage.camX
        sy = stage.camY
        time = (Math.sqrt((sx - tx) * (sx - tx) + (sy - ty) * (sy - ty)) / 120).toInt + 3
      }
      stage.camX = if (lock) tx else sx + (tx - sx) * tick / time
      stage.camY = if (lock) ty else sy + (ty - sy) * tick / time
      if (lock)
        tickInnerAnimation(stage)
      else
        Some(new Tracking(s, a, tick + 1, time, sx, sy, tick + 1 > time))
    } else
      tickInnerAnimation(stage)
  }

  private def tickInnerAnimation(stage : Stage) : Option[Animation] =
    a match {
      case Some(ap) => ap.tick(stage) match {
        case Some(newA) => Some(new Tracking(s, Some(newA), tick, time, sx, sy, lock))
        case None       => None
      }
      case None => None
    }

}

class Move(s : Sprite, tx : Int, ty : Int, time : Int, sx : Int, sy : Int, tick : Int) extends Animation {

  def this(s : Sprite, tx : Int, ty : Int) = this(s, tx, ty, (Math.sqrt((s.x - tx) * (s.x - tx) + (s.y - ty) * (s.y - ty)) / 60).toInt + 2, s.x, s.y, 0)

  def tick(stage : Stage) : Option[Animation] = {
    s.highlight = tick < time
    s.x = sx + (tx - sx) * tick / time
    s.y = sy + (ty - sy) * tick / time
    if (tick < time)
      Some(new Move(s, tx, ty, time, sx, sy, tick + 1))
    else
      None
  }
}

class Remove(s : Sprite, tick : Int) extends Animation {

  def this(s : Sprite) = this(s, 0)

  def tick(stage : Stage) : Option[Animation] = {
    s.flash = true
    if (tick > 5) {
      s.tree.parent match {
        case None    => stage.sprites = stage.sprites.filterNot(_ == s)
        case Some(p) => p.tree = Tree(p.tree.children - s, p.tree.parent)
      }
      None
    } else
      Some(new Remove(s, tick + 1))
  }
}

class Add(s : Sprite, parent : Option[Sprite], tick : Int) extends Animation {

  def this(s : Sprite, parent : Option[Sprite]) = this(s, parent, 0)
  def this(s : Sprite) = this(s, None, 0)

  def tick(stage : Stage) : Option[Animation] = {
    s.flash = true
    if (tick == 0) {
      parent match {
        case None => {
          stage.sprites = stage.sprites :+ s
          s.tree = Tree(s.tree.children, None)
        }
        case Some(p) => s.attachToParent(p)
      }
    }
    if (tick > 4) {
      s.flash = false
      None
    } else
      Some(new Add(s, parent, tick + 1))
  }
}

class Change(s : Sprite, newImg : BufferedImage, tick : Int) extends Animation {

  def this(s : Sprite, newImg : BufferedImage) = this(s, newImg, 0)

  def tick(stage : Stage) : Option[Animation] = {
    s.flash = true
    if (tick > 2) {
      s.img = newImg
    }
    if (tick > 4) {
      s.flash = false
      None
    } else
      Some(new Change(s, newImg, tick + 1))
  }
}

class Emancipate(s : Sprite) extends Animation {

  def tick(stage : Stage) : Option[Animation] =
    s.tree.parent match {
      case Some(p) => {
        s.x = s.globalX
        s.y = s.globalY
        s.detachFromParent
        stage.sprites = stage.sprites :+ s
        None
      }
      case None => None
    }
}

class Subordinate(s : Sprite, parent : Sprite) extends Animation {

  def tick(stage : Stage) : Option[Animation] = {
    s.detachFromParent
    stage.sprites = stage.sprites.filterNot(_ == s)
    s.attachToParent(parent)
    s.x = s.x - parent.globalX
    s.y = s.y - parent.globalY
    None
  }
}






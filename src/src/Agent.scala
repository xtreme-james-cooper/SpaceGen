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
import src.util.MediaProvider

class Agent(val typ : AgentType, val birth : Int, var name : String, sg : SpaceGen) {
  var location : Planet = null //TODO nuke null
  var resources : Int = 0
  var fleet : Int = 0
  var timer : Int = 0 //Actually hand-set
  var target : Option[Planet] = None
  private var sprite : Sprite = null //TODO nuke null

  sg.agents = sg.agents + this

  def getSprite : BufferedImage = typ.getSprite

  def setLocation(newLocation : Planet) : Unit =
    if (location != newLocation) {
      if (location != null) {
        var passedMe : Boolean = false
        for (ag <- sg.agents) {
          if (ag == this) {
            passedMe = true
          } else if (ag.location == location && passedMe) {
            Main.add(Stage.move(ag.sprite, ag.sprite.x - 36, ag.sprite.y))
          }
        }
      }
      location = newLocation
      location match {
        case null => {
          Main.add(Stage.tracking(sprite, Stage.remove(sprite)))
          Main.animate
        }
        case loc => {
          var locOffset : Int = getLocOffset
          if (sprite == null) {
            sprite = new Sprite(getSprite, loc.sprite.x + locOffset * 36, loc.sprite.y - 64)
            Main.add(Stage.tracking(loc.sprite, Stage.add(sprite)))
          } else {
            Main.add(Stage.tracking(sprite, Stage.move(sprite, loc.sprite.x + locOffset * 36, loc.sprite.y - 64)))
          }
          var passedMe : Boolean = false
          for (ag <- sg.agents) {
            if (ag == this) {
              passedMe = true
            } else if (ag.location == location && passedMe) {
              Main.add(Stage.move(ag.sprite, ag.sprite.x + 36, ag.sprite.y))
            }
          }
          Main.animate
        }
      }
    }

  private def getLocOffset : Int = {
    var locOffset : Int = 0
    for (ag <- sg.agents) {
      if (ag == this) {
        return locOffset
      } else if (ag.location == location) {
        locOffset = locOffset + 1
      }
    }
    locOffset
  }

}

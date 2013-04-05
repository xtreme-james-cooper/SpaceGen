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

import src.models.Artefact
import src.models.Structure
import src.models.SpecialLifeform
import src.models.Plague
import src.Planet
import src.Population
import src.runner.Main

class PlanetSprite(p : Planet) extends Sprite(p.getSprite, p.x * 240, p.y * 240) {

  var popSprites : Map[Population, List[Sprite]] = Map()
  var ownerSprite : CivSprite = null
  var lifeformSprites : Map[SpecialLifeform, Sprite] = Map()
  var structureSprites : Map[Structure, Sprite] = Map()
  var artefactSprites : Map[Artefact, Sprite] = Map()
  var plagueSprites : Map[Plague, Sprite] = Map()

  //TODO clean up control flow
  def popX(pop : Population, index : Int) : Int = {
    val step : Int = Math.min(36, 160 / p.population)
    var d : Int = 0
    for (pop2 <- p.inhabitants) {
      if (pop2 == pop) {
        return d + step * index
      } else {
        d = d + step * pop2.size
      }
    }

    0
  }

  def rearrangePopulation : Unit = {
    for {
      pop <- p.inhabitants
      sprites : List[Sprite] = popSprites.get(pop).get //TODO get
      i <- 0 until sprites.length
    } {
      Main.add(Stage.move(sprites(i), popX(pop, i), 0))
    }
    Main.animate
  }

}

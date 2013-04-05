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

package src.models

import java.awt.image.BufferedImage
import src.util.MediaProvider

sealed abstract class SpecialLifeform(val name : String, val desc : String, val imageName : String) {
  def getSprite : BufferedImage = MediaProvider.border(MediaProvider.getImage(imageName))
}

case object ULTRAVORE extends SpecialLifeform(
  "Ultravores",
  "The ultimate apex predator, the Ultravore is capable of stalking and killing even the most intelligent and well-armed prey.",
  "agents/ultravores")
case object PHARMACEUTICALS extends SpecialLifeform(
  "Pharmaceuticals",
  "Plants that contain interesting chemical compounds with medical applications.",
  "lifeforms/pharmaceuticals")
case object SHAPE_SHIFTER extends SpecialLifeform(
  "Shape-shifters",
  "A predatory creature able to mimic any other, even a sentient one.",
  "agents/shape_shifters")
case object BRAIN_PARASITE extends SpecialLifeform(
  "Brain parasites",
  "A parasitical creature able to interface with the brain of its host, enslaving it.",
  "sentients/parasites")
case object VAST_HERDS extends SpecialLifeform(
  "Vast grazing herds",
  "Untold millions of large grazing animals that provide an abundant source of food and other resources.",
  "lifeforms/vast_grazing_herds")
case object FLYING_CREATURES extends SpecialLifeform(
  "Beautiful flying creatures",
  "Fluttering fliers that display a dazzling array of colours.",
  "lifeforms/beautiful_flying_creatures")
case object OCEAN_GIANTS extends SpecialLifeform(
  "Ocean giants",
  "Huge sea creatures growing to more than a kilometre of length.",
  "lifeforms/ocean_giants")
case object LIVING_ISLANDS extends SpecialLifeform(
  "Living islands",
  "Composed of the shells of millions of small crustaceans, each of these floating islands hosts its own unique ecosystem.",
  "lifeforms/living_islands")
case object GAS_BAGS extends SpecialLifeform(
  "Gas bags",
  "Held aloft by sacs of hydrogen, these delicate creatures float about everywhere.",
  "lifeforms/gas_bags")
case object RADIOVORES extends SpecialLifeform(
  "Radiovores",
  "These small worm-like creatures derive their energy directly from exposed deposits of radioactive materials.",
  "lifeforms/radiovores")

object SpecialLifeform {

  val values : List[SpecialLifeform] =
    List(ULTRAVORE, PHARMACEUTICALS, SHAPE_SHIFTER, BRAIN_PARASITE, VAST_HERDS, FLYING_CREATURES, OCEAN_GIANTS, LIVING_ISLANDS, GAS_BAGS, RADIOVORES)

}
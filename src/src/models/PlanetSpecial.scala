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

import src.Planet

sealed abstract class PlanetSpecial(val name : String, val announcement : Planet => String, val explanation : String) {
  def appl(p : Planet) : Unit = ()
}

case object POISON_WORLD extends PlanetSpecial(
  "poison_world",
  n => n.name + " has become a poison world.",
  "Poison World: Mineral deposits produce a steady stream of toxins that makes survival extremely difficult.") {
  override def appl(p : Planet) : Unit = p.setPollution(p.getPollution + 4)
}
case object GEM_WORLD extends PlanetSpecial(
  "gem_world",
  n => "Deposits of huge gems form on " + n.name + ".",
  "Gem World: Huge, beautiful gems many metres across can be found in caves and in veins.")
case object TITANIC_MOUNTAINS extends PlanetSpecial(
  "titanic_mountains",
  n => "Titanic mountains form on " + n.name + ".",
  "Titanic Mountains: Mountain ranges so vast they poke out of the atmosphere into space.")
case object VAST_CANYONS extends PlanetSpecial(
  "vast_canyons",
  n => "Vast canyons are carved on " + n.name + ".",
  "Vast Canyons: Gashes in the planet's crust so deep they reach the hot mantle, emitting gases noxious to much life but highly nutritious to some.")
case object DEEP_IMPENETRABLE_SEAS extends PlanetSpecial(
  "deep_impenetrable_seas",
  n => "Deep impenetrable seas form on " + n.name + ".",
  "Deep Impenetrable Seas: Hundreds of kilometres down, water turns into ice from pressure.")
case object BEAUTIFUL_AURORAE extends PlanetSpecial(
  "beautiful_aurorae",
  n => "Beautiful aurorae play across the skies of " + n.name + ".",
  "Beaufiful Aurorae: The solar wind makes bright displays of flickering green at the planet's poles.")
case object GIGANTIC_CAVE_NETWORK extends PlanetSpecial(
  "gigantic_cave_network",
  n => "A gigantic network of caves forms on " + n.name + ".",
  "Gigantic Cave Network: Thousands of kilometres of partially submerged caves riddle the crust of this planet. Very easy to get lost or hide in.")
case object TIDALLY_LOCKED_TO_STAR extends PlanetSpecial(
  "tidally_locked_to_star",
  n => n.name + " becomes tidally locked to its star.",
  "Tidally Locked: This planet always presents the same side to its sun. There is perpetual day on one side, perpetual night on the other, and a ring of eternal twilight in between.")
case object MUSICAL_CAVES extends PlanetSpecial(
  "musical_caves",
  n => "The wind plays across rock formations on " + n.name + ", producing an eerie music.",
  "Musical Caves: The wind whistles through massive caves and across rocks, producing an eerie music.")
case object ICE_PLANET extends PlanetSpecial(
  "ice_planet",
  n => n.name + " becomes covered with ice.",
  "Ice Planet: Nearly the entire planet is covered in a thick sheet of ice.")
case object PERIODICAL_DARKNESS extends PlanetSpecial(
  "periodical_darkness",
  n => "Due to an accident of orbital mechanics, every 13 years, " + n.name + " is plunged into total darkness.",
  "Periodical Darkness: Every thirteen years, the orbit of this planet synchronizes with that of a planet closer to its stars, causing a night that lasts for weeks.")
case object HUGE_PLAINS extends PlanetSpecial(
  "huge_plains",
  n => "Vast empty plains form on " + n.name + ".",
  "Vast Empty Plains: Brown, dusty and featureless, their monotony is broken only by the occasional rivulet around which cluster many flying animals.")
	

object PlanetSpecial {
  
  val values : List[PlanetSpecial] = List(POISON_WORLD, GEM_WORLD, TITANIC_MOUNTAINS, VAST_CANYONS, DEEP_IMPENETRABLE_SEAS, 
      BEAUTIFUL_AURORAE, GIGANTIC_CAVE_NETWORK, TIDALLY_LOCKED_TO_STAR, MUSICAL_CAVES, ICE_PLANET, PERIODICAL_DARKNESS, HUGE_PLAINS)
  
}
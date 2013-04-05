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

sealed abstract class Cataclysm(val name : String, val desc : Planet => String)

case object NOVA extends Cataclysm(
    "nova", p => "The star of " + p.name + " goes nova, scraping the planet clean of all life!")
case object VOLCANIC_ERUPTIONS extends Cataclysm(
    "series of volcanic eruptions", p => "Massive volcanic eruptions on " + p.name + " eradicate all life on the planet!")
case object AXIAL_SHIFT extends Cataclysm(
    "shift in the planet's orbital axis", p => "A shift in the orbital axis of " + p.name + " spells doom for all life on the planet!")
case object METEORITE_IMPACT extends Cataclysm(
    "massive asteroid impact", p => "All life on " + p.name + " is killed off by a massive asteroid impact!")
case object NANOFUNGAL_BLOOM extends Cataclysm(
    "nanofungal bloom", p => "A nanofungal bloom consumes all other life on " + p.name + " before itself dying from a lack of nutrients!")
case object PSIONIC_SHOCKWAVE extends Cataclysm(
    "psionic shockwave", p => "A psionic shockwave of unknown origin passes through " + p.name + ", instantly stopping all life!")

object Cataclysm {
  
  val values : List[Cataclysm] = List(NOVA, VOLCANIC_ERUPTIONS, AXIAL_SHIFT, METEORITE_IMPACT, NANOFUNGAL_BLOOM, PSIONIC_SHOCKWAVE)
  
}
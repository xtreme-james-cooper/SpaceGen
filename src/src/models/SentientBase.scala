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

import src.CivAction
import src.BUILD_CONSTRUCTION
import src.COLONISE_PLANET
import src.EXPLORE_PLANET
import src.BUILD_MINING_BASE
import src.BUILD_WARSHIPS
import src.BUILD_MILITARY_BASE
import src.DO_RESEARCH
import src.BUILD_SCIENCE_OUTPOST

object Base {

  val values : List[Base] = List(ANTOIDS, KOBOLDOIDS, URSOIDS, DWARFOIDS, CATOIDS, TROLLOIDS, DEEP_DWELLERS, PARASITES, ROBOTS, HUMANOIDS)

}

sealed abstract class Base(val name : String, val evolvable : Boolean, val desc : String, val specialStructure : SpecialStructureType,
                           val pSuffix : String, val nameStarts : List[String], val nameEnds : List[String], val behavior : List[CivAction])

case object ANTOIDS extends Base(
  "Antoids", true,
  "Small, industrious and well-organised, these creatures' greatest weakness is their ever-ballooning population.",
  new SpecialStructureType("cluster of breeding pits"),
  " Antenna",
  List("Kak", "Krk'", "Tk", "Tch'", "Tk'k"),
  List("erlak", "kra", "hkt", "ukk", "kraa"),
  List(EXPLORE_PLANET, COLONISE_PLANET, COLONISE_PLANET, COLONISE_PLANET, BUILD_MINING_BASE, BUILD_MINING_BASE, BUILD_CONSTRUCTION))
case object KOBOLDOIDS extends Base(
  "Koboldoids", true,
  "Eat pretty much anything, including one another. Disturbingly fond of skulls.",
  new SpecialStructureType("skull pit"),
  " Claw",
  List("Grzikngh", "Brghz", "Zraa", "Klutt", "Murgezzog", "Okkog Okkog", "Frix", "Zrippo", "Zazapakka",
    "Krull", "Blorgorz", "Uzzakk", "Hittehelmettepol", "Zong", "Krghl"),
  List(" Jameson", " Smith", " Jones", " Taylor", " Brown", " Williams", " Smythe", " Clarke", " Robinson",
    " Wilson", " Johnson", " Walker", " Wood", " Hall", " Thompson"),
  List(EXPLORE_PLANET, COLONISE_PLANET, COLONISE_PLANET, COLONISE_PLANET, BUILD_MINING_BASE, BUILD_CONSTRUCTION, BUILD_CONSTRUCTION))
case object URSOIDS extends Base(
  "Ursoids", true,
  "Always at war with someone, or everyone, or one another.",
  new SpecialStructureType("barracks"),
  "muzzle",
  List("Ur", "Ber'", "Gro", "Brm'", "Or"),
  List("sus", "mog", "rr", "orr", "serk"),
  List(EXPLORE_PLANET, EXPLORE_PLANET, BUILD_MILITARY_BASE, BUILD_WARSHIPS, BUILD_WARSHIPS))
case object DWARFOIDS extends Base(
  "Dwarfoids", true,
  "Fond of drink and industry - especially building strange contraptions.",
  new SpecialStructureType("great hall"),
  "beard",
  List("Urist", "Grolin", "Gnolin", "Minin", "Balin"),
  List(" McUrist", " Thundersson", " Longbeard", " Bronzepick", " Carpwrestler"),
  List(EXPLORE_PLANET, COLONISE_PLANET, BUILD_MINING_BASE, BUILD_MINING_BASE, BUILD_MINING_BASE, DO_RESEARCH, BUILD_CONSTRUCTION))
case object CATOIDS extends Base(
  "Catoids", true,
  "Always curious and often cruel.",
  new SpecialStructureType("torture chamber"),
  "paw",
  List("Mrr", "Mrw", "Mmbrr", "Rrre", "Mee"),
  List("oaw", "ow", "orr", "reww", "ar"),
  List(EXPLORE_PLANET, COLONISE_PLANET, EXPLORE_PLANET, EXPLORE_PLANET, BUILD_WARSHIPS, BUILD_WARSHIPS))
case object TROLLOIDS extends Base(
  "Trolloids", true,
  "They have lost the ability to reproduce naturally, and compensate for this with a focus on science and paranoia.",
  new SpecialStructureType("gene library"),
  "rock",
  List("Unkut", "Rufkut", "Finkut"),
  List(" Sedimin", " Aa", " Igneous", " Geode"),
  List(EXPLORE_PLANET, BUILD_MILITARY_BASE, BUILD_MILITARY_BASE, BUILD_SCIENCE_OUTPOST, BUILD_SCIENCE_OUTPOST, DO_RESEARCH))
case object DEEP_DWELLERS extends Base(
  "Deep Dwellers", true,
  "They live in the deep oceans or in deep caves, hiding. Their presence can go unnoticed for a long time.",
  new SpecialStructureType("deep dome"),
  "crest",
  List("Zursu", "Uln", "Wi", "Paraa", "Nio"),
  List("pram", "ivex", "lon", "ix", "it"),
  List(BUILD_SCIENCE_OUTPOST, BUILD_MILITARY_BASE, BUILD_MILITARY_BASE, BUILD_MINING_BASE, BUILD_MINING_BASE, COLONISE_PLANET))
case object PARASITES extends Base(
  "Parasites", false,
  "Operate by infesting the brains of other sentients and using their bodies as vehicles.",
  new SpecialStructureType("biolab"),
  " Tentacle",
  List("Dark ", "Shining ", "Slithering ", "Grasping ", "Insidious "),
  List("Tentacle", "Tentril", "Beak", "Ovipositor", "Needle"),
  List(EXPLORE_PLANET, EXPLORE_PLANET, EXPLORE_PLANET, COLONISE_PLANET, COLONISE_PLANET, DO_RESEARCH))
case object ROBOTS extends Base(
  "Robots", false,
  "Purely mechanical lifeforms, artificially created.",
  new SpecialStructureType("repair bay"),
  " Node",
  List("Node ", "Subroutine ", "Subcomponent ", "Unit "),
  List("23/4432", "12-Theta-23", "039", "550-b", "12-a/x"),
  List(BUILD_WARSHIPS, BUILD_MINING_BASE, BUILD_CONSTRUCTION, BUILD_CONSTRUCTION, BUILD_CONSTRUCTION, BUILD_CONSTRUCTION))
case object HUMANOIDS extends Base(
  "Humanoids", true,
  "Pretty adventurous but otherwise fairly average.",
  new SpecialStructureType("space academy"),
  "beard",
  List("James T. ", "Jason ", "Annette ", "Asimov "),
  List("Asimov", "Cairn", "Runge-Kutta", "Johnson"),
  List(EXPLORE_PLANET, EXPLORE_PLANET, COLONISE_PLANET, BUILD_SCIENCE_OUTPOST, BUILD_MILITARY_BASE, BUILD_MINING_BASE, BUILD_WARSHIPS))


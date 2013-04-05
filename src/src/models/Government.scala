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

import src.BUILD_CONSTRUCTION
import src.BUILD_MILITARY_BASE
import src.BUILD_MINING_BASE
import src.BUILD_SCIENCE_OUTPOST
import src.BUILD_WARSHIPS
import src.COLONISE_PLANET
import src.CivAction
import src.DO_RESEARCH
import src.EXPLORE_PLANET

sealed abstract class Government(
  val name : String,
  val title : String,
  val maleLeader : String,
  val femaleLeader : String,
  val bombardP : Int,
  val encounterOutcomes : List[SentientEncounterOutcome],
  val behaviour : List[CivAction],
  val imageName : String)

case object DICTATORSHIP extends Government(
  "Military Dictatorship",
  "Empire",
  " the Emperor of the ",
  " the Empress of the ",
  2,
  List(EXTERMINATE, EXTERMINATE, EXTERMINATE_FAIL, IGNORE, IGNORE, IGNORE, IGNORE, IGNORE, SUBJUGATE, SUBJUGATE, SUBJUGATE, SUBJUGATE, SUBJUGATE, SUBJUGATE),
  List(EXPLORE_PLANET, EXPLORE_PLANET, COLONISE_PLANET, BUILD_SCIENCE_OUTPOST, BUILD_MINING_BASE, BUILD_MILITARY_BASE, BUILD_MILITARY_BASE, BUILD_MILITARY_BASE, DO_RESEARCH, BUILD_WARSHIPS, BUILD_WARSHIPS, BUILD_WARSHIPS, BUILD_CONSTRUCTION),
  "misc/military_dictatorship")
case object THEOCRACY extends Government(
  "Theocracy",
  "Church",
  " the Autarch of the ",
  " the Grand Matron of the ",
  4,
  List(EXTERMINATE, EXTERMINATE, EXTERMINATE_FAIL, IGNORE, IGNORE, IGNORE, SUBJUGATE, SUBJUGATE, SUBJUGATE, SUBJUGATE, SUBJUGATE),
  List(EXPLORE_PLANET, COLONISE_PLANET, COLONISE_PLANET, COLONISE_PLANET, BUILD_MINING_BASE, BUILD_MILITARY_BASE, BUILD_MILITARY_BASE, BUILD_WARSHIPS, BUILD_CONSTRUCTION, BUILD_CONSTRUCTION),
  "misc/theocracy")
case object FEUDAL_STATE extends Government(
  "Feudal State",
  "Kingdom",
  " the King of the ",
  " the Queen of the ",
  2,
  List(EXTERMINATE, EXTERMINATE_FAIL, IGNORE, IGNORE, IGNORE, IGNORE, IGNORE, SUBJUGATE, SUBJUGATE, SUBJUGATE, GIVE_FULL_MEMBERSHIP),
  List(EXPLORE_PLANET, COLONISE_PLANET, COLONISE_PLANET, BUILD_MINING_BASE, BUILD_MILITARY_BASE, DO_RESEARCH, BUILD_WARSHIPS, BUILD_WARSHIPS, BUILD_CONSTRUCTION),
  "misc/feudal_state")
case object REPUBLIC extends Government(
  "Republic",
  "Republic",
  " the President of the ",
  " the President of the ",
  1,
  List(EXTERMINATE, EXTERMINATE, EXTERMINATE_FAIL, IGNORE, IGNORE, IGNORE, IGNORE, IGNORE, SUBJUGATE, SUBJUGATE, GIVE_FULL_MEMBERSHIP, GIVE_FULL_MEMBERSHIP, GIVE_FULL_MEMBERSHIP, GIVE_FULL_MEMBERSHIP, GIVE_FULL_MEMBERSHIP, GIVE_FULL_MEMBERSHIP, GIVE_FULL_MEMBERSHIP),
  List(EXPLORE_PLANET, COLONISE_PLANET, BUILD_MINING_BASE, BUILD_MINING_BASE, BUILD_MILITARY_BASE, BUILD_SCIENCE_OUTPOST, DO_RESEARCH, BUILD_WARSHIPS, BUILD_CONSTRUCTION, BUILD_CONSTRUCTION),
  "misc/republic")

object Government {

  val values : List[Government] = List(DICTATORSHIP, THEOCRACY, FEUDAL_STATE, REPUBLIC)

}
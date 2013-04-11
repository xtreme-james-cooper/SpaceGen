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

import src.runner.Main
import src.models.Artefact
import src.models.REPUBLIC
import src.models.Art

sealed abstract class GoodCivEvent
case object GOLDEN_AGE_OF_SCIENCE extends GoodCivEvent
case object GOLDEN_AGE_OF_INDUSTRY extends GoodCivEvent
case object GOLDEN_AGE_OF_ART extends GoodCivEvent
case object POPULATION_BOOM extends GoodCivEvent
case object DEMOCRATISATION extends GoodCivEvent
case object SPAWN_ADVENTURER extends GoodCivEvent

object GoodCivEvent {

  val values : List[GoodCivEvent] = List(GOLDEN_AGE_OF_SCIENCE, GOLDEN_AGE_OF_INDUSTRY, GOLDEN_AGE_OF_ART, POPULATION_BOOM, DEMOCRATISATION, SPAWN_ADVENTURER)

  def invoke(actor : Civ, sg : SpaceGen) : Unit = sg.pick(values) match {

    case GOLDEN_AGE_OF_SCIENCE => {
      actor.setResources(actor.resources + 5)
      val ret : String = BUILD_SCIENCE_OUTPOST.i(actor, sg)
      actor.setScience(actor.science + 10)
      sg.l("The " + actor.name + " enters a golden age of science! " + ret)
      Main.confirm
    }

    case GOLDEN_AGE_OF_INDUSTRY => {
      actor.setResources(actor.resources + 10)
      val ret = BUILD_MINING_BASE.i(actor, sg)
      val ret2 = BUILD_MINING_BASE.i(actor, sg)
      sg.l("The " + actor.name + " enters a golden age of industry! " + ret + " " + ret2)
      Main.confirm
    }

    case GOLDEN_AGE_OF_ART => {
      val col : Planet = sg.pick(actor.fullColonies)
      val artT : Art = sg.pick(Art.values)
      val art : Artefact = artT.create(actor, sg)
      col.addArtefact(art)
      sg.l("Artists on " + col.name + " create a " + art.desc + ". ")
      Main.confirm
    }

    case POPULATION_BOOM => {
      for {
        col <- actor.colonies
        p <- col.inhabitants
      } {
        p.setSize(p.size + 2)
      }
      sg.l("The " + actor.name + " experiences a population boom! ")
      Main.confirm
    }

    case DEMOCRATISATION if actor.govt != REPUBLIC => {
      val oldName : String = actor.name
      for {
        c <- actor.colonies
        p <- c.inhabitants
        if !actor.fullMembers.contains(p.typ)
      } {
        actor.fullMembers = actor.fullMembers ++ List(p.asInstanceOf[Population].typ)
      }
      actor.setGovt(REPUBLIC)
      for {
        p <- actor.colonies
        pop <- p.inhabitants
      } {
        pop.addUpdateImgs
      }
      Main.animate
      sg.l("A popular movement overthrows the old guard of the " + oldName + " and declares the " + actor.name + ".")
      Main.confirm
    }

    case SPAWN_ADVENTURER if !actor.colonies.isEmpty => {
      val st : SentientType = sg.pick(actor.fullMembers)
      val name : String = "Captain " + sg.pick(st.base.nameStarts) + sg.pick(st.base.nameEnds)
      val p : Planet = sg.pick(actor.colonies)
      val ag : Agent = new Agent(ADVENTURER(actor, st), sg.year, name, sg)
      ag.fleet = 2 + sg.d(6)
      ag.resources = sg.d(6)
      ag.setLocation(p)
      sg.l(name + ", space adventurer, blasts off from " + p.name + ".")
      Main.confirm
    }

    case _ => ()

  }

}

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
import src.models.SCIENCE_LAB
import src.models.Artefact
import src.models.Device
import src.models.ROBOTS

object Science {

  def advance(actor : Civ, sg : SpaceGen) : Boolean = sg.d(9) match {
    case 0 => {
      actor.setTechLevel(actor.techLevel + 1)
      if (actor.techLevel == 10) {
        sg.l("The highly advanced technology of the " + actor.name + " allows them to transcend the bounds of this universe. They vanish instantly.")
        actor.colonies.foreach(_ transcend (sg.year))
        sg.civs = sg.civs - actor
        Main.confirm
        true
      } else
        false
    }
    case 1 => {
      // Develop new weapons systems.
      sg.l("The " + actor.name + " develop powerful new weapons.")
      actor.setWeapLevel(actor.weapLevel + 1)
      Main.confirm
      false
    }
    case 2 => {
      val srcP : Planet = actor.largestColony.get //TODO get
      if (srcP.population > 1) {
        for {
          p <- actor.reachables(sg)
          if !p.habitable && p.owner.isEmpty
        } {
          p.habitable = true

          var srcPop : Population = sg.pick(srcP.inhabitants)
          for {
            pop <- srcP.inhabitants
            if actor.fullMembers.contains(pop.typ) && pop.size > 1
          } {
            srcPop = pop
          }
          srcPop.send(p)

          p.setOwner(Some(actor))
          Main.animate(Stage.tracking(p.sprite, Stage.change(p.sprite, p.getSprite)))
          sg.l("The " + actor.name + " terraform and colonise " + p.name + ".")
          Main.confirm
          return false
        }
      }

      uplift(actor, sg)

    }
    case 3 => uplift(actor, sg)
    case 4 => robots(actor, sg)
    case 5 => {
      val target : Option[Planet] = actor.largestColony
      if (target.isDefined) {
        val name : String = sg.pick(List("Soj'r", "Monad", "Lun'hod", "Mar'er", "P'neer", "Dyad", "Triad"))
        val probe : Agent = new Agent(SPACE_PROBE(actor), sg.year, name, sg)
        probe.target = target
        probe.timer = 8 + sg.d(25)
        sg.l("The " + actor.name + " launch a space probe called " + probe.name + " to explore the galaxy.")
        Main.confirm
      }
      false
    }
    case 6 | 7 | 8 => {
      val cands : List[Planet] = actor.largestColony.get :: actor.colonies.filter(_ has (SCIENCE_LAB)) //TODO get
      val p : Planet = sg.pick(cands)
      val typ : Device = sg.pick(Device.values)
      val a : Artefact = new Artefact(sg.year, Some(actor), typ, typ.create(actor, sg))
      p.addArtefact(a)
      sg.l("The " + actor.name + " develop a " + a.typ.getName + ".")
      Main.confirm
      false
    }
  }

  private def uplift(actor : Civ, sg : SpaceGen) : Boolean = {
    for {
      p <- actor.reachables(sg)
      if p.habitable && p.owner.isEmpty && p.inhabitants.isEmpty
    } {
      val st : SentientType = SentientType.invent(sg, Some(actor), p, None)
      p.setOwner(Some(actor))
      new Population(st, 3, p)
      sg.l("The " + actor.name + " uplift the local " + st.name + " on " + p.name + " and incorporate the planet into their civilisation.")
      Main.confirm
      return false
    }

    // ROBOTS!
    robots(actor, sg)
  }

  private def robots(actor : Civ, sg : SpaceGen) : Boolean = {
    val cands : List[Planet] = actor.fullColonies.filter(p => p.inhabitants.forall(pop => pop.typ.base != ROBOTS))
    sg.pickMaybe(cands) match {
      case Some(rp) => {
        val rob : SentientType = SentientType.genRobots(sg, Some(actor), rp, None)
        sg.l("The " + actor.name + " create " + rob.name + " as servants on " + rp.name + ".")
        new Population(rob, 4, rp)
        Main.confirm
      }
      case None => ()
    }
    false
  }

}

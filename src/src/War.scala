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
import src.util.CivSprite
import src.models.MILITARY_BASE
import src.models.Ruin
import src.models.LostArtefact
import src.models.WAR
import src.models.Device
import src.models.KILLER_MEME
import src.models.ARTIFICIAL_PLAGUE
import src.models.MIND_CONTROL_DEVICE
import src.models.PLANET_DESTROYER
import src.models.TIME_MACHINE
import src.models.UNIVERSAL_COMPUTER_VIRUS
import src.models.DEEP_DWELLERS
import src.models.PARASITES
import src.models.URSOIDS
import src.models.ForReason

object DoWar {

  def doWar(actor : Civ, sg : SpaceGen) : Unit =
    if (actor.military > 0) {
      val targets : List[Planet] =
        for {
          p <- sg.planets
          if p.owner.isDefined && p.owner.get != actor && actor.relation(p.owner.get) == WAR
        } yield p
      sg.pickMaybe(targets) match {
        case Some(target) => {
          val victim : Civ = target.owner.get //TODO get? (kinda safe)
          if (actor.has(TIME_MACHINE)) {
            for (p <- victim.colonies) {
              p.deCiv(sg.year / 2, ForReason("by a time vortex"))
              p.setOwner(None)
            }
            sg.civs = sg.civs - victim
            val p : Planet = sg.pick(sg.planets)
            if (p.strata.isEmpty)
              p.strata = p.strata ++ List(new LostArtefact("lost", sg.year / 4, actor.use(TIME_MACHINE)))
            else
              p.strata = List(new LostArtefact("lost", p.strata.head.time / 2, actor.use(TIME_MACHINE))) ++ p.strata
            sg.l("The " + actor.name + " use their time machine to erase their hated enemies, the " + victim.name + ".")
            Main.confirm
          } else if (actor.has(KILLER_MEME)) {
            sg.l("The " + actor.name + " use their memetic weapon against the " + victim.name + ".")
            BadCivEvent.invoke(MASS_HYSTERIA, victim, sg)
            target.strata = target.strata ++ List(new LostArtefact("forgotten", sg.year, actor.use(KILLER_MEME)))
            Main.confirm
          } else if (actor.has(UNIVERSAL_COMPUTER_VIRUS)) {
            sg.l("The " + actor.name + " use their universal computer virus against the " + victim.name + ".")
            BadCivEvent.invoke(MARKET_CRASH, victim, sg)
            target.strata = target.strata ++ List(new LostArtefact("forgotten", sg.year, actor.use(UNIVERSAL_COMPUTER_VIRUS)))
            Main.confirm
          } else if (actor.has(ARTIFICIAL_PLAGUE)) {
            sg.l("The " + actor.name + " use their artificial plague against the " + victim.name + ".")
            BadCivEvent.invoke(SPAWN_PLAGUE, victim, sg)
            actor.use(ARTIFICIAL_PLAGUE)
            Main.confirm
          } else {
            val srcP : Planet = actor.largestColony.get //TODO get
            val fleet : Sprite = new Sprite(CivSprite.EXPEDITION, srcP.sprite.x - 48, srcP.sprite.y + 160 / 2 - 32 / 2)
            fleet.children = fleet.children + new CivSprite(actor, true)
            Main.animate(Stage.add(fleet))
            Main.animate(Stage.tracking(fleet, Stage.move(fleet, target.sprite.x - 48, target.sprite.y + 160 / 2 - 32 / 2)))
            Main.animate(Stage.track(target.sprite), Stage.remove(fleet))

            val attack : Int = actor.military * (2 + (actor.techLevel + 2 * actor.weapLevel))
            val defenseBonus = if (target.has(MILITARY_BASE)) 5 * (victim.techLevel + 2 * victim.weapLevel) else 0
            var defence : Int = target.population + defenseBonus
            if (target.has(URSOIDS.specialStructure)) {
              defence = defence + 4
            }
            val attackRoll : Int = sg.d(attack, 6)
            val defenceRoll : Int = sg.d(defence, 6)
            if (attackRoll > defenceRoll) {
              actor.setMilitary(actor.military - sg.d(actor.military / 6 + 1))
              if (sg.d(6) < actor.govt.bombardP || victim.has(PARASITES)) {
                if (actor.has(PLANET_DESTROYER)) {
                  target.deLive(sg.year, ForReason("when the planet was scoured by a superweapon of the " + actor.name))
                  sg.l("The " + actor.name + " attack " + target.name + " and use their planet destroyer to turn it into a lifeless cinder.")
                  Main.confirm
                  return
                }
                if (target.has(DEEP_DWELLERS.specialStructure)) {
                  for (st <- target.structures) {
                    if (sg.p(3)) {
                      target.strata = target.strata ++ List(new Ruin(st, sg.year, ForReason("through orbital bombardment by the " + actor.name)))
                      target.removeStructure(st)
                    }
                  }
                  sg.l("The " + actor.name + " attack " + target.name + ", a colony of the " + victim.name + ", and subject it to orbital bombardment. " +
                    "Its inhabitants hide in the dome built deep in the planet's crust and escape harm.")
                  Main.confirm
                  return
                }
                var deaths : Int = 0
                for (pop <- target.inhabitants) {
                  val pd : Int = sg.d(pop.size) + 1
                  if (pd >= pop.size)
                    target.dePop(pop, sg.year, ForReason("due to orbital bombardment by the " + actor.name))
                  else
                    pop.setSize(pop.size - pd)
                  deaths = deaths + pd
                }
                if (target.population == 0) {
                  target.deCiv(sg.year, ForReason("due to orbital bombardment by the " + actor.name))
                  sg.l("The " + actor.name + " attack and raze " + target.name + ", a colony of the " + victim.name + ".")
                } else {
                  for (st <- target.structures) {
                    if (sg.coin) {
                      target.strata = target.strata ++ List(new Ruin(st, sg.year, ForReason("through orbital bombardment by the " + actor.name)))
                      target.removeStructure(st)
                    }
                  }
                  sg.l("The " + actor.name + " attack " + target.name + ", a colony of the " + victim.name +
                    ", and subject it to orbital bombardment, killing " + deaths + " billion.")
                }
              } else {
                actor.setResources(actor.resources + victim.resources / victim.colonies.size / 2 + 1)
                victim.setResources(victim.resources - victim.resources / victim.colonies.size + 1)
                if (actor.has(MIND_CONTROL_DEVICE))
                  sg.l("The " + actor.name + " conquer " + target.name + ", a colony of the " + victim.name +
                    ", using their mind control device to gain control of the planet from orbit.")
                else {
                  for (st <- target.structures) {
                    if (sg.p(4)) {
                      target.strata = target.strata ++ List(new Ruin(st, sg.year, ForReason("during the invasion of the " + actor.name)))
                      target.removeStructure(st)
                    }
                  }
                  if (target.population > 0) {
                    val deaths = getInvasionDeaths(target, actor, sg)
                    if (deaths > 0)
                      sg.l("The " + actor.name + " conquer " + target.name + ", a colony of the " + victim.name + ", killing " + deaths + " billion in the process.")
                    else
                      sg.l("The " + actor.name + " conquer " + target.name + ", a colony of the " + victim.name + ".")
                  } else {
                    sg.l("The " + actor.name + " conquer " + target.name + ", a colony of the " + victim.name + ".")
                  }
                }

                for (a <- target.artefacts) {
                  if (a.typ.isInstanceOf[Device]) {
                    sg.l("The " + actor.name + " gain control of the " + a.typ.getName + " on " + target.name + ".")
                  }
                }

                target.setOwner(Some(actor))

                for (pop <- target.inhabitants) {
                  pop.addUpdateImgs
                }
                Main.animate
              }
            } else {
              for (st <- target.structures) {
                if (sg.p(6)) {
                  target.strata = target.strata ++ List(new Ruin(st, sg.year, ForReason("during an attack by the " + actor.name)))
                  target.removeStructure(st)
                }
              }
              actor.setMilitary(actor.military - sg.d(actor.military / 3 + 1))
              sg.l("The " + victim.name + " repel the " + actor.name + " at " + target.name + ".")
            }
            Main.confirm
          }
        }
        case None => ()
      }
    }

  private def getInvasionDeaths(target : Planet, actor : Civ, sg : SpaceGen) : Int = {
    var deaths : Int = 0
    for (pop <- target.inhabitants) {
      var pd : Int = sg.d(pop.size - pop.size / 2)
      if (pd >= target.population) {
        pd = 1
      }
      if (pd >= target.population) {
        return deaths
      }
      if (pd >= pop.size)
        target.dePop(pop, sg.year, ForReason("during the invasion of the " + actor.name))
      else
        pop.setSize(pop.size - pd)
      deaths = deaths + pd
    }
    deaths
  }
}

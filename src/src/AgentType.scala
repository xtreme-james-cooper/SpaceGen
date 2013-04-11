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
import src.models.MILITARY_BASE
import src.models.Stratum
import src.models.SHAPE_SHIFTER
import src.models.BRAIN_PARASITE
import src.models.ULTRAVORE
import src.models.Ruin
import src.models.Remnant
import src.models.Plague
import src.models.LostArtefact
import src.models.Fossil
import src.models.PEACE
import src.models.UNION
import src.models.WAR
import src.models.Artefact
import src.models.ArtefactType
import src.models.Device
import src.models.MIND_ARCHIVE
import src.models.STASIS_CAPSULE
import src.models.ContainedAgent
import src.models.ContainedST
import src.models.PIRATE_TOMB
import src.models.WRECK
import src.models.TIME_ICE
import src.models.PIRATE_HOARD
import src.models.ADVENTURER_TOMB
import src.util.MediaProvider
import java.awt.image.BufferedImage
import src.models.URSOIDS
import src.models.DEEP_DWELLERS
import src.models.ForReason
import src.models.ByPlague

object AgentType {

  val MONSTER_TYPES : List[String] = List("worm", "cube", "crystal", "jellyfish")

}

sealed abstract class AgentType(val name : String) {

  def behave(a : Agent, sg : SpaceGen) : Unit

  def describe(a : Agent, sg : SpaceGen) : String

  def getSprite : BufferedImage

}

case class PIRATE(color : String, st : SentientType) extends AgentType("PIRATE") {

  override def describe(a : Agent, sg : SpaceGen) : String = {
    val d : String = "In orbit: The pirate " + a.name + ", a " + st.name
    if (a.fleet < 2)
      d + "."
    else
      d + ", commanding a fleet of " + a.fleet + " ships."
  }

  override def behave(a : Agent, sg : SpaceGen) : Unit = {
    // move
    a.setLocation(sg.pick(sg.planets))
    val age : Int = sg.year - a.birth
    if (age > 8 + sg.d(6)) {
      sg.l("The pirate " + a.name + " dies and is buried on " + a.location.name + ".")
      val art : Artefact = new Artefact(sg.year, None, PIRATE_TOMB, "Tomb of the Pirate " + a.name)
      art.specialValue = a.resources + a.fleet
      a.location.addStrata(new LostArtefact("buried", sg.year, art))
      a.setLocation(null)
      sg.agents = sg.agents - a
      Main.confirm
    } else
      a.location.owner match {
        case Some(o) => {
          val tribute : Int = sg.d(8) + 1
          if (o.resources >= tribute && !sg.p(4)) {
            o.setResources(o.resources - tribute)
            a.resources = a.resources + tribute
            sg.l("The pirate " + a.name + " receives tribute from " + a.location.name + " of the " + o.name + ".")
          } else {
            val attack : Int = a.fleet * 4
            val defenseBonus : Int =
              if (a.location.has(MILITARY_BASE)) 5 * (o.techLevel + 2 * o.weapLevel) else 0
            var defence : Int = a.location.population + defenseBonus
            if (a.location.has(URSOIDS.specialStructure)) {
              defence = defence + 4
            }
            val attackRoll : Int = sg.d(attack, 6)
            val defenceRoll : Int = sg.d(defence, 6)
            val target : Planet = a.location
            if (attackRoll > defenceRoll) {
              if (target.has(DEEP_DWELLERS.specialStructure)) {
                for (st <- target.structures) {
                  if (sg.p(3)) {
                    target.addStrata(new Ruin(st, sg.year, ForReason("through orbital bombardment by the pirate " + a.name)))
                    target.removeStructure(st)
                  }
                }
                sg.l("The pirate " + a.name + " subjects " + target.name + " to orbital bombardment. " +
                  "Its inhabitants hide in the dome deep in the planet's crust and escape harm.")
              } else {
                var deaths : Int = 0
                for (pop <- target.inhabitants) {
                  val pd : Int = sg.d(pop.size) + 1
                  if (pd >= pop.size) {
                    target.dePop(pop, sg.year, ForReason("due to orbital bombardment by the pirate " + a.name))
                  } else {
                    pop.setSize(pop.size - pd)
                  }
                  deaths = deaths + pd
                }
                if (target.population == 0) {
                  target.deCiv(sg.year, ForReason("due to orbital bombardment by the pirate " + a.name))
                  sg.l("The pirate " + a.name + " subjects " + target.name + " to orbital bombardment.")
                } else {
                  for (st <- target.structures) {
                    if (sg.coin) {
                      target.addStrata(new Ruin(st, sg.year, ForReason("through orbital bombardment by the pirate " + a.name)))
                      target.removeStructure(st)
                    }
                  }
                  sg.l("The pirate " + a.name + " subjects " + target.name + " to orbital bombardment, killing " + deaths + " billion.")
                }
              }
            } else {
              sg.l("The " + o.name + " defeats the pirate " + a.name + ".")
              o.setResources(o.resources + a.resources / 2)
              o.setMilitary(o.military * 5 / 6)
              a.setLocation(null)
              sg.agents = sg.agents - a
            }
            Main.confirm
          }
        }
        case None => {
          // Buy more ships or leave pirate treasure.
          if (a.resources > 5) {
            if (sg.p(3)) {
              sg.l("The pirate " + a.name + " buries a hoard of treasure on " + a.location.name + ".")
              val art : Artefact = new Artefact(sg.year, None, PIRATE_HOARD, "Hoard of the Pirate " + a.name)
              art.specialValue = a.resources - 2
              a.resources = 2
              a.location.addStrata(new LostArtefact("buried", sg.year, art))
              Main.confirm
            } else {
              a.fleet = a.fleet + 1
              a.resources = a.resources - 2
            }
          }
        }
      }
  }

  override def getSprite : BufferedImage = st.getSprite(true, color, false, false)

}

case class ADVENTURER(originator : Civ, st : SentientType) extends AgentType("ADVENTURER") {

  override def describe(a : Agent, sg : SpaceGen) : String = {
    val d : String = "In orbit: The adventurer " + a.name + ", a member of the " + st.name + ", serving the " + originator.name
    if (a.fleet < 2)
      d + "."
    else
      d + ", commanding a fleet of " + a.fleet + " ships."
  }

  def encounter(a : Agent, sg : SpaceGen, ag : Agent) : Boolean = ag.typ match {
    case ROGUE_AI => {
      if (sg.coin) {
        if (a.fleet <= 3) {
          sg.l(a.name + " is killed by the rogue AI " + ag.name + ".")
          sg.agents = sg.agents - a
          a.location.addStrata(new LostArtefact("crashed", sg.year, new Artefact(sg.year, Some(originator), WRECK,
            "wreck of the flagship of " + a.name + ", destroyed by the rogue AI " + ag.name)))
          a.setLocation(null)
        } else {
          val loss : Int = sg.d(a.fleet - 1) + 1
          sg.l(a.name + " is attacked by the rogue AI " + ag.name + " and has to retreat, losing " + loss + " ships.")
          a.fleet = a.fleet - loss
          a.location.addStrata(new LostArtefact("crashed", sg.year, new Artefact(sg.year, Some(originator), WRECK,
            "shattered wrecks of " + loss + " spaceships of the fleet of " + a.name + ", destroyed by the rogue AI " + ag.name)))
        }
      } else {
        sg.l(a.name + " manages to confuse the rogue AI " + ag.name + " with a clever logic puzzle, distracting it long enough to shut it down.")
        a.resources = a.resources + 5
        sg.agents = sg.agents - ag
        ag.setLocation(null)
      }
      Main.confirm
      true
    }
    case SPACE_PROBE(_, _) => {
      if (sg.coin) {
        sg.l(a.name + " attempts to reason with the space probe " + ag.name + " but triggers its self-destruct mechanism.")
        if (sg.coin) {
          a.location.deLive(sg.year, ForReason("due to the self-destruction of the insane space probe " + ag.name))
          sg.l("The resulting shockwave exterminates all life on " + a.location.name + ".")
          sg.agents = sg.agents - a
          a.setLocation(null)
        }
      } else {
        sg.l(a.name + " successfully reasons with the insane space probe " + ag.name +
          ", which transfers its accumulated information into the fleet's data banks and then shuts down.")
        originator.setTechLevel(originator.techLevel + 3)
      }
      sg.agents = sg.agents - ag
      ag.setLocation(null)
      Main.confirm
      true
    }
    case SPACE_MONSTER(_, _) => {
      val attackRoll : Int = sg.d(a.fleet, 6)
      val defenseRoll : Int = sg.d(4, 6)
      if (attackRoll > defenseRoll) {
        sg.l(a.name + " defeats the " + ag.name + " in orbit around " + a.location.name + ".")
        sg.agents = sg.agents - ag
        ag.setLocation(null)
        a.location.owner match {
          case Some(o) => {
            sg.l("The " + o.name + " rewards the adventurer handsomely.")
            a.resources = a.resources + o.resources / 3
            o.setResources(o.resources * 2 / 3)
          }
          case None => ()
        }
      } else {
        val loss : Int = sg.d(2) + 2
        if (a.fleet - loss <= 0) {
          sg.l("The " + ag.name + " in orbit around " + a.location.name + " attacks and kills " + a.name + ".")
          sg.agents = sg.agents - a
          a.location.addStrata(new LostArtefact("crashed", sg.year, new Artefact(sg.year, Some(originator), WRECK,
            "wreck of the flagship of " + a.name + ", destroyed by a " + ag.name)))
          a.setLocation(null)
        } else {
          a.fleet = a.fleet - loss
          sg.l("The " + ag.name + " attacks the fleet of " + a.name + " near " + a.location.name + " destroying " + loss + " ships.")
          a.location.addStrata(new LostArtefact("crashed", sg.year, new Artefact(sg.year, Some(originator), WRECK,
            "shattered wrecks of " + loss + " spaceships of the fleet of " + a.name + ", destroyed by a " + ag.name)))
        }
      }
      Main.confirm
      true
    }
    case _ => false
  }

  override def behave(a : Agent, sg : SpaceGen) : Unit = {
    a.setLocation(sg.pick(sg.planets))
    val age : Int = sg.year - a.birth
    if (!sg.civs.contains(originator) || age > 8 + sg.d(6)) {
      sg.l("The space adventurer " + a.name + " dies and is buried on " + a.location.name + ".")
      val art : Artefact = new Artefact(sg.year, None, ADVENTURER_TOMB, "Tomb of " + a.name)
      a.location.addStrata(new LostArtefact("buried", sg.year, art))
      art.specialValue = a.resources / 3 + a.fleet / 5 + 1
      sg.agents = sg.agents - a
      a.setLocation(null)
      Main.confirm
    } else {

      for (ag <- sg.agents) {
        if (ag != a && ag.location == a.location) {
          if (encounter(a, sg, ag)) {
            return
          }
        }
      }

      if (sg.p(3) && a.location.owner.isDefined && a.location.owner != Some(originator) && originator.relation(a.location.owner.get) == WAR) { //TODO isdefined, get
        // Show some initiative!
        val act : String = sg.pick(List(
          " raids the treasury on ",
          " intercepts a convoy near ",
          " steals jewels from ",
          " steals a spaceship from the navy of ",
          " extorts money from "))
        sg.l(a.name + act + a.location.name + ", a planet of the enemy " + a.location.owner.get.name + ".") //TODO get? safe
        a.resources = a.resources + 2
        a.location.owner.get.setResources(a.location.owner.get.resources * 5 / 6) //TODO get? safe
        Main.confirm
      } else if (a.location.owner.isEmpty || (a.location.owner.get != originator && originator.relation(a.location.owner.get) == PEACE)) {
        // Exploration
        var rep : String = "An expedition led by " + a.name + " explores " + a.location.name + ". "
        var major : Boolean = false

        // - find artefacts
        // - exterminate bad wildlife

        def doRunAway(a : Agent, sg : SpaceGen) : Boolean = {
          for (slf <- a.location.lifeforms) {
            if (sg.coin && (slf == BRAIN_PARASITE || slf == ULTRAVORE || slf == SHAPE_SHIFTER)) {
              val monster : String = slf.name.toLowerCase
              major = true
              rep = rep + "They encounter the local " + monster
              if (sg.p(3)) {
                rep = rep + " and exterminate them. "
                a.location.removeLifeform(slf)
              } else if (a.fleet < 1) {
                rep = rep + ". In a desperate attempt to stop them, " + a.name + " activates the ship's self-destruct sequence."
                sg.agents = sg.agents - a
                a.setLocation(null)
                return true
              } else {
                rep = rep + ". In a desperate attempt to stop them, " + a.name + " has half of the exploration fleet blasted to bits."
                a.fleet = a.fleet / 2
                return true
              }
            }
          }
          false
        }

        if (doRunAway(a, sg)) {
          sg.l(rep.toString)
          Main.confirm
        } else {

          // Inhabs
          sg.pickMaybe(a.location.inhabitants) match {
            case Some(local) => {
              major = true
              rep = rep + "They trade with the local " + local.typ.name + ". "
              a.location.evoPoints = a.location.evoPoints + 5000
              a.resources = a.resources + 2
            }
            case None => ()
          }

          // Archeology!
          val p : Planet = a.location
          var stratMissing : Int = 0
          for (stratNum <- 0 until p.strata.size) {
            val stratum : Stratum = p.strata(p.strata.size - stratNum - 1 + stratMissing)
            if (sg.p(4 + stratNum * 2)) {
              stratum match {
                case _ : Fossil => {
                  rep = rep + "They discover: " + stratum.toString + " "
                  originator.setScience(originator.science + 1)
                }
                case r : Remnant => {
                  rep = rep + "They discover: " + stratum.toString + " "
                  a.resources = a.resources + 1
                  val homeP : Planet = originator.largestColony.get //TODO get
                  r.cause match {
                    case ByPlague(p) if sg.d(6) < p.transmissivity => {
                      var affects : Boolean = false
                      for (pop <- homeP.inhabitants) {
                        if (p.affects.contains(pop.typ)) {
                          affects = true
                        }
                      }

                      if (affects) {
                        homeP.addPlague(new Plague(p))
                        rep = rep + " Unfortunately, members of the expedition catch the " + p.name +
                          " from their exploration of the ancient tombs, infecting " + homeP.name + " upon their return. "
                        major = true
                      }
                    }
                    case _ => ()
                  }
                }
                case la : LostArtefact => {
                  la.artefact.typ match {
                    case PIRATE_TOMB | PIRATE_HOARD | ADVENTURER_TOMB => {
                      rep = rep + "The expedition loots the " + la.artefact.desc + ". "
                      a.resources = a.resources + la.artefact.specialValue
                      p.removeStrata(stratum)
                      stratMissing = stratMissing + 1
                    }
                    case STASIS_CAPSULE => {
                      la.artefact.creator match {
                        case Some(cre) if !sg.civs.contains(cre) => {
                          rep = rep + "They open a stasis capsule from the " + cre.name + ", which arises once more!"
                          sg.civs = sg.civs + cre
                          cre.setTechLevel(la.artefact.creatorTechLevel)
                          cre.setResources(10)
                          cre.setMilitary(10)
                          p.owner match {
                            case Some(o) => {
                              o.relations = o.relations + (cre -> WAR)
                              cre.relations = cre.relations + (o -> WAR)
                            }
                            case None => ()
                          }
                          p.setOwner(Some(cre))

                          def isInserted : Boolean = {
                            for (pop <- p.inhabitants) {
                              if (Some(pop.typ) == la.artefact.st) {
                                pop.setSize(pop.size + 3)
                                return true
                              }
                            }
                            false
                          }
                          if (!isInserted) {
                            new Population(la.artefact.st.get, 3, p) //TODO get
                          }
                          cre.birthYear = sg.year
                          p.removeStrata(stratum)
                          stratMissing = stratMissing + 1
                          if (rep.length > 0) {
                            sg.l(rep.toString)
                            Main.confirm
                          }
                          return
                        }
                        case _ => ()
                      }
                    }
                    case MIND_ARCHIVE => {
                      rep = rep + "They encounter a mind archive of the " + la.artefact.creator.get.name + //TODO get
                        " which brings new knowledge and wisdom to the " + originator.name + ". "
                      major = true
                      originator.setTechLevel(Math.max(originator.techLevel, la.artefact.creatorTechLevel))
                    }
                    case WRECK => {
                      rep = rep + "They recover: " + stratum + " "
                      p.removeStrata(stratum)
                      a.resources = a.resources + 3
                      stratMissing = stratMissing + 1
                    }
                    case _ => {
                      rep = rep + "They recover: " + stratum + " "
                      major = true
                      p.removeStrata(stratum)
                      a.resources = a.resources + 1
                      sg.pick(originator.colonies).addArtefact(la.artefact)
                      stratMissing = stratMissing + 1
                    }
                  }
                }
                case _ => ()
              }
            }
          }

          if (rep.length > 0) {
            sg.l(rep)
            Main.confirm
          }
        }

      } else if (a.location.owner == Some(originator)) {
        while (a.resources > 4) {
          a.fleet = a.fleet + 1
          a.resources = a.resources - 4
        }

        // Missions!

        val agentsList : Set[Agent] = sg.agents.filter(ag => sg.planets.exists(p => ag.location == p))

        // KILL PIRATE
        agentsList.find(ag => ag.typ.isInstanceOf[PIRATE]) match { //TODO isOf
          case Some(pir) => {
            sg.l(a.name + " is sent on a mission to defeat the pirate " + pir.name + " by the government of " + a.location.name + ".")
            if (sg.coin) {
              sg.l(a.name + " fails to find any trace of the pirate " + pir.name + ".")
            } else {
              a.setLocation(pir.location)
              sg.l(a.name + " tracks down the pirate " + pir.name + " in orbit around " + pir.location.name + ".")
              // FAIGHTH!
              val attack : Int = a.fleet * 4
              val defence : Int = pir.fleet * 3
              val attackRoll : Int = sg.d(attack, 6)
              val defenceRoll : Int = sg.d(defence, 6)
              if (attackRoll > defenceRoll) {
                sg.l(a.name + " defeats the pirate " + pir.name + " - the skies of " + a.location.name + " are safe again.")
                sg.agents = sg.agents - pir
                pir.setLocation(null)
                a.resources = a.resources + pir.resources / 2
                originator.setResources(originator.resources + pir.resources / 2)
              } else {
                if (a.fleet < 2) {
                  sg.l(a.name + " is defeated utterly by the pirate.")
                  sg.agents = sg.agents - a
                  a.setLocation(null)
                } else {
                  sg.l(a.name + " is defeated by the pirate " + pir.name + " and flees back to " + a.location.name + ".")
                  a.fleet = a.fleet / 2
                }
              }
              Main.confirm
            }
          }
          // KILL SM
          case None => agentsList.find(ag => ag.typ.isInstanceOf[SPACE_MONSTER]) match { //TODO isInstanceOf
            case Some(mon) => {
              sg.l(a.name + " is sent on a mission to defeat the " + mon.name + " at " + mon.location.name + ".")
              a.setLocation(mon.location)
              encounter(a, sg, mon)
            }
            case None => agentsList.find(ag => ag.typ == ROGUE_AI) match {
              case Some(ai) => {
                sg.l(a.name + " is sent on a mission to stop the rogue AI " + ai.name + " at " + ai.location.name + ".")
                a.setLocation(ai.location)
                encounter(a, sg, ai)
              }
              case None => agentsList.find(ag => ag.typ == SPACE_PROBE) match {
                case Some(pr) => {
                  sg.l(a.name + " is sent on a mission to stop the insane space probe " + pr.name + " threatening " + pr.location.name + ".")
                  a.setLocation(pr.location)
                  encounter(a, sg, pr)
                }
                case None => {
                  // PEACE MISSION
                  var enemy : Civ = null
                  for (c <- sg.civs) {
                    if (c != originator && originator.relation(c) == WAR) {
                      enemy = c
                    }
                  }
                  if (enemy != null && sg.p(4)) {
                    sg.l("The " + originator.name + " send " + a.name + " on a mission of peace to the " + enemy.name + ".")
                    a.setLocation(enemy.largestColony.get) //TODO get
                    if (sg.coin) {
                      sg.l("The expert diplomacy of " + a.name + " is successful: the two empires are at peace.")
                      originator.relations = originator.relations + (enemy -> PEACE)
                      enemy.relations = enemy.relations + (originator -> PEACE)
                    } else {
                      a.setLocation(originator.largestColony.get) //TODO get
                      sg.l("Unfortunately, the peace mission fails. " + a.name + " hastily retreats to " + a.location.name + ".")
                    }
                    Main.confirm
                  } else {

                    // Steal U
                    if (enemy != null) {
                      for (p <- enemy.colonies) {
                        if (!p.artefacts.isEmpty) {
                          val art : Artefact = p.artefacts.head
                          sg.l("The " + originator.name + " send " + a.name + " on a mission to steal the " + art.typ.getName + " on " + p.name + ".")
                          a.setLocation(p)
                          if (sg.coin) {
                            val lc : Planet = originator.largestColony.get //TODO get
                            sg.l(a.name + " successfully acquires the " + art.typ.getName + " and delivers it to " + lc.name + ".")
                            p.moveArtefact(art, lc)
                          } else {
                            if (sg.p(3)) {
                              sg.l("The " + enemy.name + " capture and execute " + a.name + " for trying to steal the " + art.typ.getName + ".")
                              sg.agents = sg.agents - a
                              a.setLocation(null)
                            } else {
                              a.setLocation(originator.largestColony.get) //TODO get
                              sg.l("The attempt to steal the " + art.typ.getName + " fails, and " + a.name +
                                " swiftly retreats to " + a.location.name + " to avoid capture.")
                            }
                          }
                          Main.confirm
                          return
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  override def getSprite : BufferedImage = st.getSprite(false, false)

}

case object SHAPE_SHIFTERS extends AgentType("SHAPE_SHIFTERS") {

  override def describe(a : Agent, sg : SpaceGen) : String = "A pack of shape-shifters hiding amongst the local population."

  override def behave(a : Agent, sg : SpaceGen) : Unit = {
    if (a.location.inhabitants.isEmpty) {
      sg.agents = sg.agents - a
      a.setLocation(null)
    } else if (a.location.population > 1) {
      if (sg.p(6)) {
        val victim : Population = sg.pick(a.location.inhabitants)
        if (victim.size == 1) {
          sg.l("Shape-shifters devour the last remaining " + victim.typ.name + " on " + a.location.name + ".")
          a.location.dePop(victim, sg.year, ForReason("through predation by shape-shifters"))
          Main.confirm
        } else
          victim.setSize(victim.size - 1)
      }
      if (sg.p(40)) {
        sg.l("The inhabitants of " + a.location.name + " manage to identify the shape-shifters among them and exterminate them.")
        sg.agents = sg.agents - a
        a.setLocation(null)
        Main.confirm
      }
    } else {
      sg.l("The population of " + a.location.name + " turn out to be all shape-shifters. " +
        "The colony collapses as the shape-shifters need real sentients to keep up their mimicry.")
      a.location.deCiv(sg.year, ForReason("when the entire population of the planet turned out to be shape-shifters"))
      if (!a.location.lifeforms.contains(SHAPE_SHIFTER)) {
        a.location.addLifeform(SHAPE_SHIFTER)
      }
      sg.agents = sg.agents - a
      a.setLocation(null)
      Main.confirm
    }
  }

  override def getSprite : BufferedImage = MediaProvider.border(MediaProvider.getImage("agents/shape_shifters"))

}

case object ULTRAVORES extends AgentType("ULTRAVORES") {

  override def describe(a : Agent, sg : SpaceGen) : String = "A pack of ultravores, incredibly dangerous predators."

  override def behave(a : Agent, sg : SpaceGen) : Unit = {
    if (a.location.inhabitants.isEmpty || a.location.owner.isEmpty) {
      sg.agents = sg.agents - a
      a.setLocation(null)
    } else if (sg.p(6)) {
      if (a.location.population > 1) {
        val victim : Population = sg.pick(a.location.inhabitants)
        if (victim.size == 1) {
          sg.l("A billion " + victim.typ.name + " on " + a.location.name + " are devoured by ultravores.")
          a.location.dePop(victim, sg.year, ForReason("through predation by ultravores"))
        } else {
          victim.setSize(victim.size - 1)
        }
      } else {
        sg.l("Ultravores devour the final inhabitants of " + a.location.name + ".")
        a.location.deCiv(sg.year, ForReason("through predation by ultravores"))
        Main.confirm
      }
      if (sg.p(3) && a.location.owner.isDefined) {
        for {
          p <- a.location.owner.get.fullColonies //TODO get? safe
          if sg.agents.forall(ag => ag.typ != ULTRAVORES || ag.location != p)
        } {
          val ag : Agent = new Agent(ULTRAVORES, sg.year, "Hunting pack of Ultravores", sg, p)
          return
        }
      }
    }
  }

  override def getSprite : BufferedImage = MediaProvider.border(MediaProvider.getImage("agents/ultravores"))

}

case class SPACE_MONSTER(mType : String, color : String) extends AgentType("SPACE_MONSTER") {

  override def describe(a : Agent, sg : SpaceGen) : String = "In orbit: A " + a.name + " threatening the planet."

  override def behave(a : Agent, sg : SpaceGen) : Unit = {
    if (sg.p(500)) {
      sg.l("The " + a.name + " devours all life on " + a.location.name + ".")
      a.location.deLive(sg.year, ForReason("due to the attack of a " + a.name))
    } else if (sg.p(8) && a.location.population > 2) {
      val t : Population = sg.pick(a.location.inhabitants)
      if (t.size == 1) {
        sg.l("The " + a.name + " devours the last of the local " + t.typ.name + " on " + a.location.name + ".")
        a.location.dePop(t, sg.year, ForReason("due to predation by a " + a.name))
        Main.confirm
      } else {
        sg.l("The " + a.name + " devours one billion " + t.typ.name + " on " + a.location.name + ".")
        t.setSize(t.size - 1)
      }
    } else if (sg.p(20)) {
      sg.l("The " + a.name + " leaves the orbit of " + a.location.name + " and heads back into deep space.")
      sg.agents = sg.agents - a
      a.setLocation(null)
      Main.confirm
    }
  }

  override def getSprite : BufferedImage = MediaProvider.border(MediaProvider.tint(MediaProvider.getImage("agents/" + mType), MediaProvider.TINTS(color)))

}

case class SPACE_PROBE(originator : Civ, target : Planet) extends AgentType("SPACE_PROBE") {

  override def describe(a : Agent, sg : SpaceGen) : String = "In orbit: The insane space probe " + a.name + " threatening the planet."

  override def behave(a : Agent, sg : SpaceGen) : Unit = {
    if (a.location == null) {
      a.timer = a.timer - 1
      if (a.timer == 0) {
        a.setLocation(target)
        sg.l("The space probe " + a.name + " returns to " + a.location.name + ".")
        if (a.location.owner == Some(originator)) {
          sg.l("The " + originator.name + " gains a wealth of new knowledge as a result.")
          originator.setTechLevel(originator.techLevel + 3)
          sg.agents = sg.agents - a
          a.setLocation(null)
        } else {
          sg.l("Unable to contact the " + originator.name + " that launched it, the probe goes insane.")
          Main.confirm
        }
      }
    } else if (sg.p(8) && a.location.population > 2) {
      val t : Population = sg.pick(a.location.inhabitants)
      if (t.size == 1) {
        sg.l("The insane space probe " + a.name + " bombards " + a.location.name + ", wiping out the local " + t.typ.name + ".")
        a.location.dePop(t, sg.year, ForReason("due to bombardment by the insane space probe " + a.name))
      } else {
        sg.l("The insane space probe " + a.name + " bombards " + a.location.name + ", killing one billion " + t.typ.name + ".")
        t.setSize(t.size - 1)
      }
    } else if (sg.p(40)) {
      sg.l("The insane space probe " + a.name + " crashes into " + a.location.name + ", wiping out all life on the planet.")
      a.location.deLive(sg.year, ForReason("due to the impact of the space probe " + a.name))
      sg.agents = sg.agents - a
      a.setLocation(null)
      Main.confirm
    }
  }

  override def getSprite : BufferedImage = MediaProvider.border(MediaProvider.getImage("agents/space_probe"))

}

case object ROGUE_AI extends AgentType("ROGUE_AI") {

  override def describe(a : Agent, sg : SpaceGen) : String = "In orbit: The rogue AI " + a.name + "."

  override def behave(a : Agent, sg : SpaceGen) : Unit = {
    if (a.timer > 0) {
      a.timer = a.timer - 1
      if (a.timer == 0) {
        a.setLocation(sg.pick(sg.planets))
        sg.l("The rogue AI " + a.name + " reappears on " + a.location.name + ".")
      }
    } else {
      if (sg.p(10)) {
        a.setLocation(sg.pick(sg.planets))
      }
      if (sg.p(50)) {
        sg.l("The rogue AI " + a.name + " vanishes without a trace.")
        a.timer = 40 + sg.d(500)
        a.setLocation(null)
      } else if (sg.p(80)) {
        sg.l("The rogue AI " + a.name + " vanishes without a trace.")
        sg.agents = sg.agents - a
        a.setLocation(null)
      } else {

        if (sg.p(40)) {
          for (ag <- sg.agents) {
            if (ag != a && ag.location == a.location) {
              var art : Artefact = null
              ag.typ match {
                case ADVENTURER(_, _) => {
                  sg.l("The rogue AI " + a.name + " encases the adventurer " + ag.name + " in a block of time ice.")
                  art = new Artefact(sg.year, "the rogue AI " + a.name, TIME_ICE, "block of time ice encasing " + ag.name)
                }
                case PIRATE(_, _) => {
                  sg.l("The rogue AI " + a.name + " encases the pirate " + ag.name + " in a block of time ice.")
                  art = new Artefact(sg.year, "the rogue AI " + a.name, TIME_ICE, "block of time ice encasing the pirate " + ag.name)
                }
                case SHAPE_SHIFTERS => {
                  sg.l("The rogue AI " + a.name + " encases the shape-shifters on" + a.location.name + " in a block of time ice.")
                  art = new Artefact(sg.year, "the rogue AI " + a.name, TIME_ICE, "block of time ice, encasing a group of shape-shifters")
                }
                case ULTRAVORES => {
                  sg.l("The rogue AI " + a.name + " encases a pack of ultravores on " + a.location.name + " in a block of time ice.")
                  art = new Artefact(sg.year, "the rogue AI " + a.name, TIME_ICE, "block of time ice, encasing a pack of ultravores")
                }
                case ROGUE_AI => {
                  val newName : String = "Cluster " + sg.r.nextInt(100)
                  sg.l("The rogue AI " + a.name + " merges with the rogue AI " + a.location.name + " into a new entity called " + newName + ".")
                  a.name = newName
                  sg.agents = sg.agents - ag
                  a.setLocation(null)
                  Main.confirm
                  return
                }
                case _ => ()
              }
              if (art != null) {
                art.contained = Some(ContainedAgent(ag))
                a.location.addArtefact(art)
                sg.agents = sg.agents - ag
                a.setLocation(null)
                Main.confirm
                return
              }
            }
          }
        }

        // Random mischief!
        a.location.owner match {
          case Some(o) => {
            if (sg.p(60)) {
              val st : SentientType = sg.pick(o.fullMembers)
              val name : String = sg.pick(st.base.nameStarts) + sg.pick(st.base.nameEnds)
              val title : String = o.govt.maleLeader
              val ar : Artefact = new Artefact(sg.year, "the rogue AI " + a.name, TIME_ICE, "block of time ice, encasing " + name + "," + title + o.name)
              ar.contained = Some(ContainedST(st))
              a.location.addArtefact(ar)
              sg.l("The rogue AI " + a.name + " encases " + name + ", " + title + " of the " + o.name + ", in a block of time ice.")
              Main.confirm
              return
            }
            if (sg.p(60)) {
              sg.l("The rogue AI " + a.name + " crashes the " + a.location.name + " stock exchange.")
              o.setResources(o.resources / 2)
              return
            }
            if (sg.p(30)) {
              val dt : Device = sg.pick(Device.values)
              if (dt != STASIS_CAPSULE && dt != MIND_ARCHIVE) {
                val dev : Artefact = new Artefact(sg.year, "the rogue AI " + a.name, dt, dt.create(null, sg))
                a.location.addArtefact(dev)
                sg.l("The rogue AI " + a.name + " presents the inhabitants of " + a.location.name + " with a gift: a " + dev.typ.getName + ".")
                Main.confirm
              }
              return
            }
            if (sg.p(20) && !a.location.artefacts.isEmpty) {
              val art : Artefact = sg.pick(a.location.artefacts) //TODO pickMaybe
              val t : Planet = sg.pick(sg.planets)
              sg.l("The rogue AI " + a.name + " steals the " + art.desc + " on " + a.location.name + " and hides it on " + t.name + ".")
              a.location.removeArtefact(art)
              t.addStrata(new LostArtefact("hidden", sg.year, art))
              Main.confirm
              return
            }
          }
          case None => ()
        }
        if (!a.location.inhabitants.isEmpty) {
          if (sg.p(40)) {
            val pl : Plague = new Plague(sg)
            pl.affects = pl.affects :+ a.location.inhabitants.head.typ
            for (pop <- a.location.inhabitants.tail) {
              if (sg.coin) {
                pl.affects = pl.affects :+ pop.typ
              }
            }
            a.location.addPlague(pl)
            sg.l("The rogue AI " + a.name + " infects the inhabitants of " + a.location.name + " with " + pl.desc + ".")
            Main.confirm
            return
          }
          if (a.location.population > 2 && sg.p(25)) {
            for {
              t <- sg.planets
              if t.habitable && t.owner.isEmpty
            } {
              val victim : Population = sg.pick(a.location.inhabitants)
              victim.send(t)
              sg.l("The rogue AI " + a.name + " abducts a billion " + victim.typ.name +
                " from " + a.location.name + " and dumps them on " + t.name + ".")
              Main.confirm
              return
            }
          }
        }

        if (a.location.habitable && sg.p(200) && a.location.owner.isEmpty) {
          val st : SentientType = SentientType.invent(sg, None, a.location, Some("They were created by the rogue AI " + a.name + " in " + sg.year + "."))
          sg.l("The rogue AI " + a.name + " uplifts the local " + st.name + " on " + a.location.name + ".")
          new Population(st, 3 + sg.d(3), a.location)
          Main.confirm
        } else if (a.location.habitable && sg.p(250) && a.location.owner.isEmpty) {
          val st : SentientType = SentientType.genRobots(sg, None, a.location, Some("They were created by the rogue AI " + a.name + " in " + sg.year + "."))
          sg.l("The rogue AI " + a.name + " creates " + st.name + " on " + a.location.name + ".")
          new Population(st, 3 + sg.d(3), a.location)
          Main.confirm
        } else if (!a.location.habitable && sg.p(500)) {
          sg.l("The rogue AI " + a.name + " terraforms " + a.location.name + ".")
          Main.confirm
        } else if (sg.p(300) && a.location.habitable) {
          sg.l("The rogue AI " + a.name + " releases nanospores on " + a.location.name + ", destroying all life on the planet.")
          a.location.deLive(sg.year, ForReason("due to nanospores relased by " + a.name))
          Main.confirm
        } else if (sg.civs.size > 1 && sg.p(250)) {
          val c : Civ = sg.pick(sg.civs)
          val c2 : Civ = sg.pick(sg.civs - c)
          if (c.relation(c2) == PEACE) {
            sg.l("The rogue AI " + a.name + " incites war between the " + c.name + " and the " + c2.name + ".")
            c.relations = c.relations + (c2 -> WAR)
            c2.relations = c2.relations + (c -> WAR)
          } else {
            sg.l("The rogue AI " + a.name + " brokers peace between the " + c.name + " and the " + c2.name + ".")
            c.relations = c.relations + (c2 -> PEACE)
            c2.relations = c2.relations + (c -> PEACE)
          }
          Main.confirm
        }
      }
    }
  }

  override def getSprite : BufferedImage = MediaProvider.border(MediaProvider.getImage("agents/rogue_ai"))

}

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
import src.util.Stage
import src.util.Sprite
import src.util.CivSprite
import src.models.Structure
import src.models.StructureType
import src.models.Standard
import src.models.SCIENCE_LAB
import src.models.MINING_BASE
import src.models.MILITARY_BASE
import src.models.Stratum
import src.models.BRAIN_PARASITE
import src.models.PHARMACEUTICALS
import src.models.SHAPE_SHIFTER
import src.models.ULTRAVORE
import src.models.SentientEncounterOutcome
import src.models.SUBJUGATE
import src.models.IGNORE
import src.models.GIVE_FULL_MEMBERSHIP
import src.models.EXTERMINATE_FAIL
import src.models.EXTERMINATE
import src.models.Ruin
import src.models.Remnant
import src.models.Plague
import src.models.LostArtefact
import src.models.REPUBLIC
import src.models.Government
import src.models.Fossil
import src.models.Outcome
import src.models.PEACE
import src.models.UNION
import src.models.WAR
import src.models.Artefact
import src.models.ArtefactType
import src.models.LIVING_WEAPON
import src.models.MIND_ARCHIVE
import src.models.STASIS_CAPSULE
import src.models.ADVENTURER_TOMB
import src.models.PIRATE_TOMB
import src.models.PIRATE_HOARD
import src.models.WRECK
import src.models.PARASITES
import src.models.DEEP_DWELLERS
import src.models.ForReason
import src.models.ByPlague

sealed abstract class CivAction {

  def i(actor : Civ, sg : SpaceGen) : String

  def invoke(actor : Civ, sg : SpaceGen) : Unit = {
    val rep : String = i(actor, sg)
    if (rep.length > 0) {
      sg.l(rep)
      Main.confirm
    }
  }

  def buildOutpost(st : StructureType, actor : Civ, sg : SpaceGen) : String = {
    if (actor.resources >= 5) {
      for {
        tries <- 0 until 20
        // Pick a planet.
        p : Planet = sg.pick(actor.reachables(sg))
        if (p.owner.isEmpty && p.inhabitants.isEmpty) || p.owner == Some(actor)
        if !p.has(st)
        if p.structures.size < 5
      } {
        val srcP : Planet = actor.closestColony(p)
        Main.animate(Stage.track(srcP.sprite))
        if (p.owner != Some(actor)) {
          val expedition : Sprite = new Sprite(CivSprite.EXPEDITION, srcP.sprite.x - 48, srcP.sprite.y + 160 / 2 - 32 / 2)
          expedition.children = expedition.children + new CivSprite(actor, true)
          Main.animate(Stage.add(expedition))
          Main.animate(Stage.tracking(expedition, Stage.move(expedition, p.sprite.x - 48, p.sprite.y + 160 / 2 - 32 / 2)))
          Main.animate(Stage.track(p.sprite), Stage.remove(expedition))
        }

        if (p.owner != Some(actor)) {
          p.setOwner(Some(actor))
        }
        actor.setResources(actor.resources - 5)
        p.addStructure(new Structure(st, actor, sg.year))
        return "The " + actor.name + " build a " + st.name + " on " + p.name + "."
      }
    }
    ""
  }

  def buildColonyStructure(st0 : StructureType, actor : Civ, sg : SpaceGen) : String =
    if (actor.resources >= 8) {
      val st : StructureType = if (sg.p(3)) sg.pick(sg.pick(actor.fullMembers).specialStructures__) else st0
      val cands : List[Planet] = actor.colonies.filter(p => !p.isOutpost && !p.has(st) && p.structures.size < 5)
      sg.pickMaybe(cands) match {
        case None => ""
        case Some(p) => {
          Main.animate(Stage.track(p.sprite))
          if (p.owner != Some(actor)) {
            p.setOwner(Some(actor))
          }
          actor.setResources(actor.resources - 8)
          p.addStructure(new Structure(st, actor, sg.year))
          actor.decrepitude = actor.decrepitude - 3
          "The " + actor.name + " build a " + st.name + " on " + p.name + "."
        }
      }
    } else
      ""

}

case object EXPLORE_PLANET extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = {
    var expedition : Sprite = null
    // Pick a planet to explore.
    val p : Planet = sg.pick(actor.reachables(sg))
    val srcP : Planet = actor.closestColony(p)
    if (p.owner == Some(actor)) {
      Main.animate(Stage.track(p.sprite))
    } else {
      Main.animate(Stage.track(srcP.sprite))
      expedition = new Sprite(CivSprite.EXPEDITION, srcP.sprite.x - 48, srcP.sprite.y + 160 / 2 - 32 / 2)
      expedition.children = expedition.children + new CivSprite(actor, true)
      Main.animate(Stage.add(expedition))
      Main.animate(Stage.tracking(expedition, Stage.move(expedition, p.sprite.x - 48, p.sprite.y + 160 / 2 - 32 / 2)))
      Main.animate(Stage.track(p.sprite))
    }
    val ret : String = expedite(actor, sg, p, srcP)
    if (expedition != null) {
      Main.animate(Stage.remove(expedition))
    }
    ret
  }

  def expedite(actor : Civ, sg : SpaceGen, p : Planet, srcP : Planet) : String = {
    var rep : String = ""
    p.owner match {
      case Some(other) if other != actor => {
        // They meet a civ.
        rep = rep + "The " + actor.name + " send a delegation to " + p.name + ". "
        var outcome : Outcome = Diplomacy.meet(actor, other, sg)
        rep = rep + outcome.desc(other, actor.relation(other)) + " "
        if (outcome == UNION && (actor.has(PARASITES) || other.has(PARASITES))) {
          outcome = PEACE
        }
        if (outcome == UNION) {
          sg.civs = sg.civs - other
          val newGovt : Government = sg.pick(List(actor.govt, other.govt))
          for (c <- other.colonies) {
            c.setOwner(Some(actor))
          }
          actor.setResources(actor.resources + other.resources)
          actor.setScience(actor.science + other.science)
          for (st <- other.fullMembers) {
            if (!actor.fullMembers.contains(st)) {
              actor.fullMembers = actor.fullMembers :+ st
            }
          }
          var newRels : Map[Civ, Outcome] = Map()
          for (c <- sg.civs) {
            if (c != actor && c != other) {
              if (actor.relation(c) == WAR || other.relation(c) == WAR) {
                newRels = newRels + (c -> WAR)
                c.relations = c.relations + (actor -> WAR)
              } else {
                newRels = newRels + (c -> PEACE)
                c.relations = c.relations + (actor -> PEACE)
              }
            }
          }
          actor.relations = newRels
          actor.setGovt(newGovt)
          rep = rep + "The two civilizations combine into the " + actor.name + ". "
        } else {
          if (actor.relation(other) != outcome) {
            actor.relations = actor.relations + (other -> outcome)
            other.relations = other.relations + (actor -> outcome)
          } else {
            rep = ""
          }
        }
      }
      case _ => {
        var major : Boolean = false
        rep = rep + "The " + actor.name + " explore " + p.name + ". "
        val base : String = rep.toString

        // The wildlife.
        for (slf <- p.lifeforms) {
          slf match {
            case BRAIN_PARASITE => {
              if (sg.p(3)) {
                val victimP : Planet = sg.pick(actor.colonies)
                val stolenResources : Int = actor.resources / actor.colonies.size
                val newCiv : Civ = new Civ(sg.year, List(SentientType.PARASITE), victimP, sg.pick(Government.values), stolenResources, sg)
                rep = rep + "The expedition encounters brain parasites. Upon their return to " + victimP.name +
                  ", the parasites take over the brains of the planet's inhabitants, creating the " + newCiv.name + "."
                victimP.setOwner(Some(newCiv))
                newCiv.relations = newCiv.relations + (actor -> WAR)
                actor.relations = actor.relations + (newCiv -> WAR)
                return rep
              }
            }
            case PHARMACEUTICALS => {
              rep = rep + "The expedition encounters plants with useful pharmaceutical properties. "
              actor.setScience(actor.science + 4)
              major = true
            }
            case SHAPE_SHIFTER => {
              if (sg.p(3)) {
                major = true
                val victimP : Planet = sg.pick(actor.colonies)
                rep = rep + "Shape-shifters impersonate the crew of the expedition. Upon their return to " + victimP.name + " they merge into the population."
                val ag : Agent = new Agent(SHAPE_SHIFTERS, sg.year, "Pack of Shape-Shifters", sg)
                ag.setLocation(victimP)
              }
              return rep
            }
            case ULTRAVORE => {
              val victimP : Planet = sg.pick(actor.colonies)
              if (victimP.population < 2 || sg.coin) {
                if (sg.p(10)) {
                  rep = rep + "The expedition captures an ultravore. The science of the " + actor.name + " fashions it into a living weapon of war. "
                  actor.largestColony.get.addArtefact(new Artefact(sg.year, Some(actor), LIVING_WEAPON, LIVING_WEAPON.create(actor, sg))) //TODO get
                  major = true
                }
              } else {
                major = true
                rep = rep + "An ultravore stows away on the expedition's ship. Upon their return to " + victimP.name + " it escapes and multiplies."
                val ag : Agent = new Agent(ULTRAVORES, sg.year, "Hunting Pack of Ultravores", sg)
                ag.setLocation(victimP)
                return rep
              }
            }
            case _ => ()
          }
        }

        // The locals.
        p.owner match {
          case None =>
            for (pop <- p.inhabitants) {
              major = true
              if (pop.typ.base == DEEP_DWELLERS) {
                rep = rep + "They remain unaware of the Deep Dweller culture far beneath. "
              } else {
                val seo : SentientEncounterOutcome = sg.pick(actor.govt.encounterOutcomes)
                rep = rep + seo.desc(pop)

                seo match {
                  case EXTERMINATE => {
                    val kills : Int = sg.d(3) + 1
                    if (kills >= pop.size) {
                      p.dePop(pop, sg.year, ForReason("a campaign of extermination by " + actor.name))
                      rep = rep + " and wipe them out. "
                    } else {
                      pop.setSize(pop.size - kills)
                      rep = rep + ", killing " + kills + " billion."
                    }
                  }
                  case EXTERMINATE_FAIL => {
                    val newCiv : Civ = new Civ(sg.year, List(pop.typ), p, REPUBLIC, 1, sg)
                    pop.setSize(pop.size + 1)
                    actor.relations = actor.relations + (newCiv -> WAR)
                    newCiv.relations = newCiv.relations + (actor -> WAR)
                    rep = rep + ", but their campaign fails disastrously. The local " + pop.typ.name +
                      " steal their technology and establish themselves as the " + newCiv.name + "."
                    return rep
                  }
                  case GIVE_FULL_MEMBERSHIP => {
                    if (!actor.fullMembers.contains(pop.typ)) {
                      actor.fullMembers = actor.fullMembers :+ pop.typ
                      actor.updateName
                      rep = rep + " They now call themselves the " + actor.name + "."
                    }
                    p.setOwner(Some(actor))
                    for (p2 <- p.inhabitants) {
                      p2.addUpdateImgs
                    }
                    Main.animate
                  }
                  case SUBJUGATE => {
                    p.setOwner(Some(actor))
                    for (p2 <- p.inhabitants) {
                      p2.addUpdateImgs
                    }
                    Main.animate
                  }
                  case IGNORE => ()
                }
                rep = rep + " "
              }
            }
          case Some(o) => ()
        }

        // The strata.
        var stratOffset : Int = 1
        for (stratNum <- 0 until p.strata.size) {
          val stratum : Stratum = p.strata(p.strata.size - stratNum - stratOffset) //TODO error location!
          if (sg.p(4 + stratNum * 2)) {
            stratum match {
              case _ : Fossil => {
                rep = rep + "They discover: " + stratum.toString + " "
                actor.setScience(actor.science + 1)
              }
              case r : Remnant => {
                rep = rep + "They discover: " + stratum.toString + " "
                actor.setResources(actor.resources + 1)
                actor.setScience(actor.science + 1)
                val homeP : Planet = actor.largestColony.get //TODO get
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
                      rep = rep + " Unfortunately, they catch the " + p.name +
                        " from their exploration of the ancient tombs, infecting " + homeP.name + " upon their return."
                      major = true
                    }
                  }
                  case _ => ()
                }
              }
              case ruin : Ruin => {
                rep = rep + "They discover: " + stratum.toString + " "
                if (ruin.structure.typ.isInstanceOf[Standard]) {
                  ruin.structure.typ match {
                    case MILITARY_BASE => {
                      actor.setMilitary(actor.military + 1)
                      actor.setScience(actor.science + 1)
                      actor.setResources(actor.resources + 1)
                    }
                    case SCIENCE_LAB => actor.setScience(actor.science + 3)

                    case MINING_BASE => actor.setResources(actor.resources + 5)
                    case _           => actor.setResources(actor.resources + 2)
                  }
                }
              }
              case la : LostArtefact => {
                la.artefact.typ match {
                  case PIRATE_TOMB | PIRATE_HOARD | ADVENTURER_TOMB => {
                    rep = rep + "They loot the " + la.artefact.desc + ". "
                    actor.setResources(actor.resources + la.artefact.specialValue)
                    p.strata = p.strata.filter(_ != stratum)
                    return rep
                  }
                  case STASIS_CAPSULE =>
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
                        def doInsert : Boolean = {
                          for (pop <- p.inhabitants) {
                            if (Some(pop.typ) == la.artefact.st) {
                              pop.setSize(pop.size + 3)
                              return true
                            }
                          }
                          false
                        }
                        val inserted : Boolean = doInsert
                        if (!inserted) {
                          new Population(la.artefact.st.get, 3, p) //TODO get
                        }
                        cre.birthYear = sg.year
                        p.strata = p.strata.filter(_ != stratum)
                        return rep
                      }
                      case _ => ()
                    }
                  case MIND_ARCHIVE => {
                    rep = rep + "They encounter a mind archive of the " + la.artefact.creator.get.name + " which brings them new knowledge and wisdom. " //TODO get 
                    major = true
                    actor.setTechLevel(Math.max(actor.techLevel, la.artefact.creatorTechLevel))
                  }
                  case WRECK => {
                    rep = rep + "They recover: " + stratum + " "
                    p.strata = p.strata.filter(_ != stratum)
                    actor.setResources(actor.resources + 3)
                    stratOffset = stratOffset + 1
                  }
                  case _ => {
                    rep = rep + "They recover: " + stratum + " "
                    major = true
                    p.strata = p.strata.filter(_ != stratum)
                    sg.pick(actor.colonies).addArtefact(la.artefact)
                    stratOffset = stratOffset + 1
                  }
                }
              }
            }
          }
        }
      }
    }
    rep
  }
}

case object COLONISE_PLANET extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = {
    var rep : String = ""
    if (actor.resources >= 6 && actor.population >= 2) {
      val srcP : Planet = actor.largestColony.get //TODO get
      if (srcP.population > 1) {
        for (tries <- 0 until 20) {
          // Pick a planet to colonise.
          val p : Planet = sg.pick(actor.reachables(sg))
          if (p.habitable && (p.owner.isEmpty || p.owner == Some(actor)) && (p.owner != Some(actor) || p.population == 0)) {
            // Who shall the colonists be?

            actor.setResources(actor.resources - 6)
            p.setOwner(Some(actor))
            rep = rep + "The " + actor.name + " colonise " + p.name + ". "
            var ne : Boolean = false
            if (!p.inhabitants.isEmpty) {
              rep = rep + "Of the natives of that planet, "
              ne = true
            }
            var first : Boolean = true
            var updNeeded : Boolean = false
            for (nativeP <- p.inhabitants) {
              if (!first) {
                rep = rep + ", "
                if (p.inhabitants.indexOf(nativeP) == p.inhabitants.size - 1) {
                  rep = rep + "and "
                }
              }
              rep = rep + "the " + nativeP.toUnenslavedString

              sg.pick(actor.govt.encounterOutcomes) match {
                case EXTERMINATE | EXTERMINATE_FAIL => {
                  p.dePop(nativeP, sg.year, ForReason("through the actions of the " + actor.name))
                  rep = rep + " are exterminated"
                }
                case IGNORE | SUBJUGATE => {
                  rep = rep + " are enslaved"
                  nativeP.addUpdateImgs
                }
                case GIVE_FULL_MEMBERSHIP => {
                  rep = rep + " are given full membership in the " + actor.name
                  if (!actor.fullMembers.contains(nativeP.typ)) {
                    actor.fullMembers = actor.fullMembers :+ nativeP.typ
                    updNeeded = true
                  }
                  nativeP.addUpdateImgs
                }
              }

              first = false
            }
            Main.animate
            if (ne) {
              rep = rep + ". "
            }
            if (updNeeded) {
              actor.updateName
              rep = rep + " They now call themselves the " + actor.name + "."
            }
            var srcPop : Population = null
            for (pop <- srcP.inhabitants) {
              if (actor.fullMembers.contains(pop.typ) && pop.size > 1) {
                srcPop = pop
              }
            }
            if (srcPop == null) {
              srcPop = sg.pick(srcP.inhabitants)
            }
            srcPop.send(p)
            return rep
          }
        }
      }
    }
    rep
  }
}

case object BUILD_SCIENCE_OUTPOST extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = buildOutpost(SCIENCE_LAB, actor, sg)
}

case object BUILD_MILITARY_BASE extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = buildOutpost(MILITARY_BASE, actor, sg)
}

case object BUILD_MINING_BASE extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = buildOutpost(MINING_BASE, actor, sg)
}

case object DO_RESEARCH extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = {
    if (actor.resources != 0) {
      val res : Int = Math.min(actor.resources, sg.d(6))
      actor.setResources(actor.resources - res)
      actor.setScience(actor.science + res)
    }
    ""
  }
}

case object BUILD_WARSHIPS extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = {
    if (actor.resources >= 3) {
      val res : Int = Math.min(actor.resources, sg.d(6) + 2)
      actor.setResources(actor.resources - res)
      actor.setMilitary(actor.military + res)
    }
    ""
  }
}

case object BUILD_CONSTRUCTION extends CivAction {
  override def i(actor : Civ, sg : SpaceGen) : String = buildColonyStructure(sg.pick(Standard.COLONY_ONLY), actor, sg)
}


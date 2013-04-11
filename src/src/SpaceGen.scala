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

import scala.util.Random
import src.util.Stage
import src.runner.Main
import src.util.Names
import src.models.Structure
import src.models.SKULL_PILE
import src.models.SCIENCE_LAB
import src.models.MINING_BASE
import src.models.MILITARY_BASE
import src.models.VAST_HERDS
import src.models.SpecialLifeform
import src.models.Ruin
import src.models.Remnant
import src.models.PlanetSpecial
import src.models.POISON_WORLD
import src.models.GEM_WORLD
import src.models.Plague
import src.models.LostArtefact
import src.models.Government
import src.models.Fossil
import src.models.Cataclysm
import src.models.MASTER_COMPUTER
import src.models.UNIVERSAL_ANTIDOTE
import src.models.MIND_READER
import src.models.STASIS_CAPSULE
import src.models.ROBOTS
import src.models.ANTOIDS
import src.models.KOBOLDOIDS
import src.models.PARASITES
import src.models.TROLLOIDS
import src.models.ForReason
import src.models.ByCataclysm
import src.models.ByPlague

class SpaceGen(seed : Long) {

  //all real
  val r : Random = new Random(seed)
  var log : List[String] = Nil
  var planets : List[Planet] = Nil
  var civs : Set[Civ] = Set()
  var historicalCivNames : Set[String] = Set()
  var agents : Set[Agent] = Set()
  var historicalSentientNames : Set[String] = Set()
  var turnLog : List[String] = Nil
  var clearTurnLogOnNewEntry : Boolean = false
  var hadCivs : Boolean = false
  var yearAnnounced : Boolean = false
  var year : Int = 0
  var age : Int = 1

  def init : Unit = {
    Main.animate(Stage.delay(10))
    l("IN THE BEGINNING, ALL WAS DARK.")
    l("THEN, PLANETS BEGAN TO FORM:")
    val np : Int = 6 + d(4, 6)
    var sb : String = ""
    for (i <- 0 until np) {
      val p : Planet = new Planet(r, this)
      sb = sb + p.name + (if (i == np - 1) "" else ", ")
      planets = planets :+ p
      Main.add(Stage.add(p.sprite))
    }
    Main.animate
    l(sb)
    Main.confirm
  }

  def checkCivDoom(c : Civ) : Boolean =
    if (c.fullColonies.isEmpty) {
      l("The " + c.name + " collapses.")
      for (out <- c.colonies) {
        out.deCiv(year, ForReason("during the collapse of the " + c.name))
      }
      Main.confirm
      true
    } else if (c.colonies.size == 1 && c.colonies.head.population == 1) {
      val remnant : Planet = c.colonies.head
      l("The " + c.name + " collapses, leaving only a few survivors on " + remnant.name + ".")
      remnant.setOwner(None)
      Main.confirm
      true
    } else
      false

  private def planetTick(planet : Planet) : Unit = {
    if (p(2500)) {
      val col : String = pick(Names.COLORS)
      val mType : String = pick(AgentType.MONSTER_TYPES)
      val mName : String = "giant spaceborne " + col.toLowerCase + " " + mType
      l("A " + mName + " appears from the depths of space and menaces the skies of " + planet.name + ".")
      val m : Agent = new Agent(SPACE_MONSTER(mType, col), year, mName, this, planet)
      Main.confirm
    }

    if ((planet.population > 12 || (planet.population > 7 && p(10)) && planet.getPollution < 4)) {
      planet.setPollution(planet.getPollution + 1)
    }
    for (pop <- planet.inhabitants) {
      if (planet.owner.isEmpty && p(100) && pop.typ.base != ROBOTS && pop.typ.base != PARASITES) {
        val nst : SentientType = pop.typ.mutate(this, None)
        l("The " + pop.typ.name + " on " + planet.name + " mutate into " + nst.name + ".")
        pop.typ = nst
        pop.update
        Main.confirm
      }
      val roll : Int = d(6)
      if (roll < planet.getPollution) {
        planet.setPollution(planet.getPollution - 1)
        if (pop.size == 1) {
          pop.eliminate
          planet.dePop(pop, year, ForReason("from the effects of pollution"))
          l(pop.typ.name + " have died out on " + planet.name + "!")
          Main.confirm
          return
        } else {
          pop.setSize(pop.size - 1)
        }
      } else {
        if (roll == 6 || (pop.typ.base == ANTOIDS && roll > 3) || (planet.owner.isDefined && roll == 5) ||
          (planet.has(ANTOIDS.specialStructure) && roll > 2)) {
          pop.setSize(pop.size + 1)
        }
      }
      if (pop.typ.base == KOBOLDOIDS && p(10) && planet.has(KOBOLDOIDS.specialStructure)) {
        pop.setSize(pop.size + 1)
        l("The skull pit on " + pop.typ.name + " excites the local " + planet.name + " into a sexual frenzy.")
      }
      if (pop.size > 3 && pop.typ.base == KOBOLDOIDS && p(20)) {
        l("The " + pop.typ.name + " on " + planet.name + " devour one billion of their own kind in a mad frenzy of cannibalism!")
        if (planet.owner.isDefined && !planet.has(SKULL_PILE) && planet.structures.size < 5) {
          planet.addStructure(new Structure(SKULL_PILE, planet.owner.get, year)) //TODO get? safe
          l("The " + pop.typ.name + " erect a pile of skulls on " + planet.name + "!")
          Main.confirm
        }
      }

      for (plague <- planet.plagues) {
        if (plague.affects.contains(pop.typ)) {
          if (d(12) < plague.lethality) {
            if (pop.size <= 1) {
              planet.dePop(pop, year, ByPlague(new Plague(plague)))
              l("The " + pop.typ.name + " on " + planet.name + " have been wiped out by the " + plague.name + "!")
            } else {
              pop.setSize(pop.size - 1)
            }
          }
        } else {
          if (d(12) < plague.mutationRate && pop.typ.base != ROBOTS) {
            plague.affects = plague.affects :+ pop.typ
            l("The " + plague.name + " mutates to affect " + pop.typ.name + ".")
          }
        }
      }
    }

    for (plague <- planet.plagues) {
      if (d(12) < plague.curability) {
        planet.removePlague(plague)
        l(plague.name + " has been eradicated on " + planet.name + ".")
      } else {
        if (d(12) < plague.transmissivity) {
          val target : Planet = pick(planets)
          var canJump : Boolean = false
          for (pop <- target.inhabitants) {
            if (plague.affects.contains(pop.typ)) {
              canJump = true
            }
          }
          if (canJump) {
            var matches : Boolean = false
            for (p2 <- target.plagues) {
              if (p2.name.equals(plague.name)) {
                for (st <- plague.affects) {
                  if (!p2.affects.contains(st)) {
                    p2.affects = p2.affects :+ st
                  }
                  matches = true
                }
              }
            }
            if (!matches) {
              target.addPlague(new Plague(plague))
            }
          }
        }
      }
    }
  }

  private def doCivEvent(c : Civ) : Unit = {
    val cAge : Int = year - c.birthYear
    if (cAge > 5) c.decrepitude = c.decrepitude + 1
    if (cAge > 15) c.decrepitude = c.decrepitude + 1
    if (cAge > 25) c.decrepitude = c.decrepitude + 1
    if (cAge > 40) c.decrepitude = c.decrepitude + 1
    if (cAge > 60) c.decrepitude = c.decrepitude + 1

    if (p(3)) {
      val evtTypeRoll : Int = d(6)
      var good : Boolean =
        if (c.decrepitude < 5) evtTypeRoll <= 5
        else if (c.decrepitude < 17) evtTypeRoll >= 4
        else if (c.decrepitude < 25) evtTypeRoll == 6
        else evtTypeRoll == 6

      var bad : Boolean =
        if (c.decrepitude < 5) false
        else if (c.decrepitude < 17) evtTypeRoll == 1
        else if (c.decrepitude < 25) evtTypeRoll < 3
        else evtTypeRoll < 5

      if (good) {
        GoodCivEvent.invoke(c, this)
      }
      if (checkCivDoom(c)) {
        civs = civs - c
      } else {
        if (bad) {
          BadCivEvent.invoke(c, this)
        }

        if (checkCivDoom(c)) {
          civs = civs - c
        } else
          DoWar.doWar(c, this)
      }
    } else
      DoWar.doWar(c, this)

  }

  private def movePop(col : Planet, c : Civ) : Unit =
    for {
      pop <- col.inhabitants
      if pop.size > 1
    } {
      pop.send(c.leastPopulousFullColony)
      return
    }

  def tick : Unit = {
    turnLog = Nil
    year = year + 1
    yearAnnounced = false
    if (!hadCivs && !civs.isEmpty) {
      l("WE ENTER THE " + Names.nth(age).toUpperCase + " AGE OF CIVILISATION")
      Main.confirm
    }
    if (hadCivs && civs.isEmpty) {
      age = age + 1
      l("WE ENTER THE " + Names.nth(age).toUpperCase + " AGE OF DARKNESS")
      Main.confirm
    }
    hadCivs = !civs.isEmpty

    for (planet <- planets) {
      planetTick(planet)
    }

    // TICK CIVS
    for (c <- civs) {
      if (checkCivDoom(c)) {
        civs = civs - c
      } else {
        var newRes : Int = 0
        var newSci : Int = 1
        for (col <- c.colonies) {
          if (c.has(UNIVERSAL_ANTIDOTE)) {
            for (p <- col.plagues) {
              l("The " + p.name + " on " + col.name + " is cured by the universal antidote.")
            }
            col.clearPlagues
          }
          if (col.population > 7 || (col.population > 4 && p(3))) {
            col.evoPoints = 0
            col.setPollution(col.getPollution + 1)
          }
          if (p(6) && col.population > 4 && col != c.leastPopulousFullColony && c.leastPopulousFullColony.population < col.population - 1) {
            movePop(col, c)
          }
          if (c.has(MIND_READER) && p(4)) {
            for (pop <- col.inhabitants) {
              pop.setSize(pop.size + 1)
            }
          }
          if (col.population == 0 && !col.isOutpost) {
            col.deCiv(year, ForReason(""))
          } else {
            if (col.population > 0) {
              newRes = newRes + 1
              if (col.lifeforms.contains(VAST_HERDS)) {
                newRes = newRes + 1
              }
            }
            if (col.specials.contains(GEM_WORLD)) newRes = newRes + 1
            if (col.has(MINING_BASE)) newRes = newRes + 1
            if (col.has(SCIENCE_LAB)) newSci = newRes + 2
            if (col.has(PARASITES.specialStructure)) newSci = newSci + 2
            if (col.has(TROLLOIDS.specialStructure)) newSci = newSci + 2
          }
        }

        if (checkCivDoom(c)) {
          civs = civs - c
        } else {

          if (c.has(MASTER_COMPUTER)) {
            newRes = newRes + 2
            newSci = newSci + 3
          }

          c.setResources(c.resources + newRes)

          val lead : SentientType = pick(c.fullMembers)
          pick(lead.base.behavior).invoke(c, this)
          if (checkCivDoom(c)) {
            civs = civs - c
          } else {
            pick(c.govt.behaviour).invoke(c, this)
            if (checkCivDoom(c)) {
              civs = civs - c
            } else {

              c.setScience(c.science + newSci)

              if (c.science > c.nextBreakthrough) {
                c.setScience(c.science - c.nextBreakthrough)
                if (!Science.advance(c, this)) {
                  c.nextBreakthrough = Math.min(500, c.nextBreakthrough * 3 / 2)
                  doCivEvent(c)
                }
              } else
                doCivEvent(c)
            }
          }
        }
      }
    }

    // TICK AGENTS
    for (a <- agents) {
      a.typ.behave(a, this)
    }
    for (a <- agents) {
      if (a.typ.isInstanceOf[ADVENTURER]) { //TODO isOf
        a.typ.behave(a, this)
      }
    }

    // TICK PLANETS
    for (pl <- planets) {
      if (pl.habitable && p(500)) {
        val c : Cataclysm = pick(Cataclysm.values)
        val civ : Option[Civ] = pl.owner
        l(c.desc(pl))
        pl.deLive(year, ByCataclysm(c))
        civ match {
          case Some(civ) if (checkCivDoom(civ)) => civs = civs - civ
          case _                                => ()
        }
        Main.confirm
      } else {

        if (p(200) && pl.getPollution > 1 && !pl.specials.contains(POISON_WORLD)) {
          l("Pollution on " + pl.name + " abates.")
          pl.setPollution(pl.getPollution - 1)
        }

        if (p(300 + 5000 * pl.specials.size)) {
          val ps : PlanetSpecial = pick(PlanetSpecial.values)
          if (!pl.specials.contains(ps)) {
            pl.specials = pl.specials :+ ps
            ps.appl(pl)
            l(ps.announcement(pl))
            if (pl.specials.size == 1) {
              Main.animate(Stage.tracking(pl.sprite, Stage.change(pl.sprite, pl.getSprite)))
              Main.confirm
            }
          }
        }
        pl.evoPoints = pl.evoPoints + d(6) * d(6) * d(6) * d(6) * d(6) * 3 * (6 - pl.getPollution)
        if (pl.evoPoints > pl.evoNeeded && p(12) && pl.getPollution < 2) {
          pl.evoPoints = 0
          if (!pl.habitable) {
            pl.habitable = true
            Main.animate(Stage.tracking(pl.sprite, Stage.change(pl.sprite, pl.getSprite)))
            l("Life arises on " + pl.name + ".")
            Main.confirm
          } else {
            if (!pl.inhabitants.isEmpty && coin) {
              if (pl.owner.isEmpty) {
                // Do the civ thing.
                val g : Government = pick(Government.values)
                val starter : Population = pick(pl.inhabitants) //TODO pull out as pickMaybe
                starter.setSize(starter.size + 1)
                val c : Civ = new Civ(year, List(starter.typ), pl, g, d(3), this)
                l("The " + starter.typ.name + " on " + pl.name + " achieve spaceflight and organise as a " + g.name + ", the " + c.name + ".")
                pl.updatePopImages
                Main.animate
                Main.confirm
              }
            } else {
              if (p(3) || pl.lifeforms.size >= 3) {
                // Sentient!
                val st : SentientType = SentientType.invent(this, None, pl, None)
                new Population(st, 2 + d(1), pl)
                l("Sentient " + st.name + " arise on " + pl.name + ".")
                Main.confirm
              } else {
                // Some special creature.
                val slf : SpecialLifeform = pick(SpecialLifeform.values)
                if (!pl.lifeforms.contains(slf)) {
                  pl.addLifeform(slf)
                  l(slf.name + " evolve on " + pl.name + ".")
                  Main.confirm
                }
              }
            }
          }
        }
      }
    }

    // Erosion
    for {
      pl <- planets
      s <- pl.strata
    } {
      val sAge : Int = year - s.time + 1
      s match {
        case s : Fossil if p(12000 / sAge + 800) => pl.strataRemove(s)
        case s : LostArtefact if s.artefact.typ != STASIS_CAPSULE && p(10000 / sAge + 500) => pl.strataRemove(s)
        case s : Remnant if p(4000 / sAge + 400) => pl.strataRemove(s)
        case s : Ruin =>
          if (s.structure.typ == MILITARY_BASE || s.structure.typ == MINING_BASE || s.structure.typ == SCIENCE_LAB) {
            if (p(1000 / sAge + 150))
              pl.strataRemove(s)
          } else if (p(3000 / sAge + 300))
            pl.strataRemove(s)
        case _ => ()
      }
    }
  }

  def describe : String = {
    var sb : String = ""

    // Critters
    val sts : Set[SentientType] =
      (for {
        p <- planets
        pop <- p.inhabitants
      } yield pop.typ).toSet

    if (sts.size > 0) {
      sb = sb + "SENTIENT SPECIES:\n"
    }
    for (st <- sts) {
      sb = sb + st.name + ": " + st.getDesc + "\n"
    }

    if (civs.size > 0) {
      sb = sb + "\nCIVILISATIONS:\n"
    }
    for (c <- civs) {
      sb = sb + c.fullDesc(this) + "\n"
    }

    sb = sb + "PLANETS:\n"
    for (p <- planets) {
      sb = sb + p.fullDesc(this) + "\n"
    }

    sb
  }

  def pickMaybe[T](ts : Set[T]) : Option[T] = pickMaybe(ts.toList)
  def pickMaybe[T](ts : List[T]) : Option[T] = if (ts.isEmpty) None else Some(ts(r.nextInt(ts.length)))
  def pick[T](ts : Set[T]) : T = pick(ts.toList)
  def pick[T](ts : List[T]) : T = ts(r.nextInt(ts.length))
  
  def l(s : String) : Unit = {
    if (clearTurnLogOnNewEntry) {
      turnLog = Nil
      clearTurnLogOnNewEntry = false
    }
    if (!yearAnnounced) {
      yearAnnounced = true
      l(year + ":")
    }
    log = log :+ s
    turnLog = turnLog :+ s
  }

  def coin : Boolean = r.nextBoolean
  def p(n : Int) : Boolean = d(n) == 0

  def d(n : Int) : Int = r.nextInt(n)

  def d(rolls : Int, n : Int) : Int = {
    var sum : Int = 0
    for (roll <- 0 until rolls) sum = sum + d(n)
    sum
  }

}

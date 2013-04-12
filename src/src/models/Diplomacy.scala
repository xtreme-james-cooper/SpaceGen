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

import src.Civ
import src.SpaceGen

object Diplomacy {

  private def TABLE(a : Government, b : Government) : List[Outcome] = (a, b) match {
    case (DICTATORSHIP, DICTATORSHIP) => List(WAR, WAR, WAR, PEACE, PEACE, PEACE)
    case (DICTATORSHIP, THEOCRACY)    => List(WAR, WAR, WAR, WAR, PEACE, PEACE)
    case (DICTATORSHIP, FEUDAL_STATE) => List(WAR, WAR, WAR, WAR, PEACE, PEACE)
    case (DICTATORSHIP, REPUBLIC)     => List(WAR, WAR, WAR, PEACE, PEACE, PEACE)
    case (THEOCRACY, DICTATORSHIP)    => List(WAR, WAR, WAR, WAR, PEACE, PEACE)
    case (THEOCRACY, THEOCRACY)       => List(WAR, WAR, WAR, WAR, WAR, PEACE)
    case (THEOCRACY, FEUDAL_STATE)    => List(WAR, WAR, WAR, PEACE, PEACE, PEACE)
    case (THEOCRACY, REPUBLIC)        => List(WAR, WAR, WAR, PEACE, PEACE, PEACE)
    case (FEUDAL_STATE, DICTATORSHIP) => List(WAR, WAR, WAR, WAR, PEACE, PEACE)
    case (FEUDAL_STATE, THEOCRACY)    => List(WAR, WAR, WAR, PEACE, PEACE, PEACE)
    case (FEUDAL_STATE, FEUDAL_STATE) => List(WAR, WAR, WAR, PEACE, PEACE, UNION)
    case (FEUDAL_STATE, REPUBLIC)     => List(WAR, WAR, PEACE, PEACE, PEACE, PEACE)
    case (REPUBLIC, DICTATORSHIP)     => List(WAR, WAR, WAR, PEACE, PEACE, PEACE)
    case (REPUBLIC, THEOCRACY)        => List(WAR, WAR, WAR, PEACE, PEACE, PEACE)
    case (REPUBLIC, FEUDAL_STATE)     => List(WAR, WAR, PEACE, PEACE, PEACE, PEACE)
    case (REPUBLIC, REPUBLIC)         => List(WAR, PEACE, PEACE, PEACE, PEACE, UNION)
  }

  def meet(a : Civ, b : Civ, sg : SpaceGen) : Outcome =
    if (a.fullMembers.head.base == URSOIDS || b.fullMembers.head.base == URSOIDS)
      WAR
    else {
      val o : Outcome = sg.pick(TABLE(a.govt, b.govt))
      if (o == WAR && a.has(MIND_READER) && sg.coin)
        PEACE
      else if (o == WAR && b.has(MIND_READER) && sg.coin)
        PEACE
      else
        o
    }

}

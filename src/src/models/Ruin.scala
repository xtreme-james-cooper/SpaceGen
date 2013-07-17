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

import src.SpaceGen

class Ruin(val structure : Structure, ruinTime : Int, cause : DisappearanceCause) extends Stratum(ruinTime) {

  override def toString : String = "The ruins of a " + structure + ", destroyed in " + ruinTime + " " +
    (cause match {
      case ForReason(r)   => r
      case ByCataclysm(c) => "by a " + c.name
      case Transcended    => "?????????" //TODO eliminate?
      case ByPlague(p)    => "?????????" //TODO eliminate? "from the " + p.name???
    }) + "."

  override def shouldErode(sg : SpaceGen) : Boolean =
    if (structure.typ == MILITARY_BASE || structure.typ == MINING_BASE || structure.typ == SCIENCE_LAB)
      sg.p(1000 / (sg.year - time + 1) + 150)
    else
      sg.p(3000 / (sg.year - time + 1) + 300)

}

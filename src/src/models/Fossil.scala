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

class Fossil(fossil : SpecialLifeform, fossilisationTime : Int, cat : Option[Cataclysm]) extends Stratum(fossilisationTime) {

  override def toString : String =
    "Fossils of " + fossil.name.toLowerCase + " that went extinct in " + fossilisationTime +
      (cat match {
        case None    => ""
        case Some(c) => " due to a " + c.name
      }) + "."

  override def shouldErode(sg : SpaceGen) : Boolean = sg.p(12000 / (sg.year - time + 1) + 800)

}
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

object Postfix {

  val values : List[Postfix] = List(S_5, S_3, EYES, TAILS)

}

sealed abstract class Postfix(val name : String)
case object S_5 extends Postfix("with Five-fold Symmetry")
case object S_3 extends Postfix("with Threefold Symmetry")
case object EYES extends Postfix("with Giant Eyes")
case object TAILS extends Postfix("with Long Tails")

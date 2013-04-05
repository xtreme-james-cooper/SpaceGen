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

import src.Population

sealed abstract class SentientEncounterOutcome(val desc : Population => String)

case object SUBJUGATE extends SentientEncounterOutcome(p => "They subjugate the local " + p.typ.getName + ".")
case object GIVE_FULL_MEMBERSHIP extends SentientEncounterOutcome(p => "They incorporate the local " + p.typ.getName + " into their civilization as equals.")
case object IGNORE extends SentientEncounterOutcome(p => "They ignore the local " + p.typ.getName + ".")
case object EXTERMINATE extends SentientEncounterOutcome(p => "They mount a campaign of extermination against the local " + p.typ.getName + ".")
case object EXTERMINATE_FAIL extends SentientEncounterOutcome(p => "They attempt to exterminate the local " + p.typ.getName + ".")

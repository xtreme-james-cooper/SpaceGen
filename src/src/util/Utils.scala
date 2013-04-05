/**
Copyright 2012 David Stark

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/

package src.util

import java.io.File
import scala.util.Random
import src.models.PlanetSpecial

object Utils {
	/** @return The folder the game jar is in. */
	def getGameFolder : File = new File(Utils.getClass.getProtectionDomain.getCodeSource.getLocation.toURI).getAbsoluteFile.getParentFile

	def shuffle[A](l : List[A], r : Random) : List[A] = {
	  var newList : List[A] = Nil
	  for (i <- l) {
	    newList = insert(newList, i, r.nextInt(newList.size + 1))
	  }
	  newList
	}
	  
	private def insert[A](l : List[A], i : A, n : Int) : List[A] = l.take(n) ++ List(i) ++ l.drop(n)
	
}
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

package src.util

import java.awt.{ Color, Graphics2D, GraphicsConfiguration, Rectangle, Transparency }
import java.awt.geom.Rectangle2D
import java.awt.image.{ BufferedImage, WritableRaster }
import java.io.File
import java.lang.ref.SoftReference
import javax.imageio.ImageIO
import java.awt.RenderingHints

/** Helper class for loading images and sounds. */
object MediaProvider {

  val BORDER : Color = new Color(20, 20, 20)

  val TINTS : Map[String, Color] = Map(
    "Red" -> new Color(255, 0, 0, 160),
    "Green" -> new Color(0, 255, 0, 160),
    "Blue" -> new Color(50, 50, 255, 160),
    "Orange" -> new Color(255, 127, 0, 160),
    "Yellow" -> new Color(255, 255, 0, 160),
    "Black" -> new Color(0, 0, 0, 160),
    "White" -> new Color(255, 255, 255, 220),
    "Purple" -> new Color(200, 0, 255, 160),
    "Grey" -> new Color(127, 127, 127, 160))

  /** Map for caching images. */
  private var images : Map[String, SoftReference[BufferedImage]] = Map()

  /** GraphicsConfiguration for properly formatting images. */
  private var config : GraphicsConfiguration = null

  def createInstance(c : GraphicsConfiguration) : Unit = config = c

  def createImage(width : Int, height : Int, transparency : Int) : BufferedImage = synchronized {
    config.createCompatibleImage(width, height, transparency)
  }

  /** Creates a scaled version of the given image. */
  def scale(src : BufferedImage, sz : Int) : BufferedImage = {
    val s2 : BufferedImage = MediaProvider.createImage(sz, sz, Transparency.BITMASK)
    val g : Graphics2D = s2.createGraphics
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_NEAREST_NEIGHBOR)
    g.scale(sz * 1.0 / src.getWidth, sz * 1.0 / src.getHeight)
    g.drawImage(src, 0, 0, null)
    s2
  }

  /**
   * Loads an image from in-jar resource, relative to com/metalbeetle/defaultgame/images.
   * @param The image name. Assuming extension of jpg unless png extension is supplied.
   * @return The image, or null on failure.
   */
  def getImage(name : String) : BufferedImage = synchronized {
    getImage(name, Transparency.TRANSLUCENT)
  }

  def getImage(name : String, transparency : Int) : BufferedImage = synchronized {
    if (images.contains(name)) {
      val r : SoftReference[BufferedImage] = images(name)
      val img : BufferedImage = r.get
      if (img != null) {
        img
      } else {
        val img : BufferedImage = readImage(name, transparency)
        images = images + (name -> new SoftReference(img))
        img
      }
    } else {
      val img : BufferedImage = readImage(name, transparency)
      images = images + (name -> new SoftReference(img))
      img
    }
  }

  def readImage(name : String, transparency : Int) : BufferedImage = synchronized {
    var extName : String = name

    if (!extName.endsWith(".jpg")) {
      extName = extName + ".png"
    }

    var img : BufferedImage = null
    try {
      img = ImageIO.read(new File(new File(Utils.getGameFolder, "images"), extName))
    } catch {
      case _ : Exception => () /* feh */
    }
    try {
      val fixedImg : BufferedImage = config.createCompatibleImage(img.getWidth, img.getHeight, transparency)
      val fig : Graphics2D = fixedImg.createGraphics
      fig.drawImage(img, 0, 0, null)
      fig.dispose
      fixedImg.flush
      fixedImg
    } catch {
      case _ : Exception => null
    }
  }

  def border(src : BufferedImage) : BufferedImage = border(src, BORDER)
  
  def border(src : BufferedImage, borderC : Color) : BufferedImage = {
    val w : Int = src.getWidth 
    val h : Int = src.getHeight
    val dst : BufferedImage = createImage(w + 2, h + 2, Transparency.BITMASK)
    val g : Graphics2D = dst.createGraphics
    g.drawImage(src, 1, 1, null)
    val ar : WritableRaster = src.getAlphaRaster
    for {
      y <- 0 until h + 2
      x <- 0 until w + 2
      if y == 0 || y > h || x == 0 || x > h || ar.getSample(x - 1, y - 1, 0) <= 0
    } {
      def border : Boolean = {
        for {
          dy <- -2 until 1
          dx <- -2 until 1
          if y + dy >= 0 && y + dy < h && x + dx >= 0 && x + dx < w && ar.getSample(x + dx, y + dy, 0) > 0
        } return true
        false
      }
      if (border) {
        dst.setRGB(x, y, borderC.getRGB)
      }
    }
    dst
  }

  def mask(src : BufferedImage, fillC : Color) : BufferedImage = {
    val w : Int = src.getWidth
    val h : Int = src.getHeight
    val dst : BufferedImage = createImage(w, h, Transparency.BITMASK)
    val ar : WritableRaster = src.getAlphaRaster
    for {
      y <- 0 until h
      x <- 0 until w
      if ar.getSample(x, y, 0) > 0
    } dst.setRGB(x, y, fillC.getRGB)
    dst
  }

  def tint(src : BufferedImage, tint : Color) : BufferedImage = {
    val w : Int = src.getWidth
    val h : Int = src.getHeight
    val dst : BufferedImage = createImage(w, h, src.getTransparency)
    val ar : WritableRaster = src.getAlphaRaster
    val a : Int = tint.getAlpha
    val r : Int = tint.getRed * a
    val g : Int = tint.getGreen * a
    val b : Int = tint.getBlue * a
    val na : Int = 255 - a

    for {
      y <- 0 until h
      x <- 0 until w
    } {
      // Need to extract alpha value from alpha raster because getRGB is broken.
      val c : Color = new Color(src.getRGB(x, y))
      val c2 = new Color(
        (c.getRed * na + (r * c.getRed) / 256) / 256,
        (c.getGreen * na + (g * c.getGreen) / 256) / 256,
        (c.getBlue * na + (b * c.getBlue) / 256) / 256,
        ar.getSample(x, y, 0))
      dst.setRGB(x, y, c2.getRGB)
    }
    dst
  }

}
package ilda

import java.io.{File, PrintWriter}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Paths}

import scala.collection.immutable.Seq

case class Point2D(x: Int, y: Int, laserOn: Boolean)

/**
  * Created by g.dancewicz on 19/04/17.
  */
class IldaFileProcessor {

  private val NrOfPointsByteIndex = 8+8+8
  private val TotelFramesIndex = 8 + 8 + 8 + 2 + 2
  private val FrameNrIndex = 8 + 8 + 8 + 2
  private val HeaderIndex = 32
  private val LaserOffGcode = "G00"
  private val LaserOnGcode = "G01"

  def processIldaFile (inputIldaFilePath: String, outputGcodeFilePath: String, scaleDivider: Int)= {
    val ildaBinaryFile: Array[Byte] = readILDAFileAsBytes(inputIldaFilePath)
    val header = readILDAHeader(ildaBinaryFile)
    val nrOfPoints = readNrOfPoints(header)
    val frameNr = readFrameNr(header)
    val totalFrames = readTotalFrames(header)
    val pointsList = readDataRecords(ildaBinaryFile, nrOfPoints)
    val onlyPositivePointsList = shiftToPositiveSquare(pointsList)
    writeGcodeOutput(onlyPositivePointsList, scaleDivider, outputGcodeFilePath)
  }


  private def convert2BytesToShort(highB: Byte, lowB: Byte): Int = {
    val byteBuffer = ByteBuffer.allocateDirect(2);
    // by choosing big endian, high order bytes must be put
    // to the buffer before low order bytes
    byteBuffer.order(ByteOrder.BIG_ENDIAN)
    byteBuffer.put(Byte.box(highB))
    byteBuffer.put(Byte.box(lowB))
    byteBuffer.flip()
    byteBuffer.getShort()
  }

  private def readILDAFileAsBytes(inputIldaFilePath : String) : Array[Byte] = {
    Files.readAllBytes(Paths.get(inputIldaFilePath))
  }

  private def readILDAHeader(ildaBinaryFile : Array[Byte]): Array[Byte] = {
    ildaBinaryFile.take(HeaderIndex)
  }

  private def readNrOfPoints(header : Array[Byte]) = {
    val nrOfPointsArr = header.drop(NrOfPointsByteIndex).take(2)
    convert2BytesToShort(nrOfPointsArr(0), nrOfPointsArr(1))
  }

  private def readTotalFrames(header : Array[Byte]) = {
    val totalFramesArr = header.drop(TotelFramesIndex).take(2)
    convert2BytesToShort(totalFramesArr(0), totalFramesArr(1))
  }

  private def readFrameNr(header : Array[Byte]) = {
    val frameNrArr = header.drop(FrameNrIndex).take(2)
    convert2BytesToShort(frameNrArr(0), frameNrArr(1))
  }

  private def readDataRecords(ildaBinaryFile : Array[Byte], nrOfPoints : Int): List[Point2D] = {
    val dataRecords: Iterator[Point2D] = ildaBinaryFile.slice(HeaderIndex, (nrOfPoints*6)+HeaderIndex).grouped(6).map(
      record => Point2D(convert2BytesToShort(record(0), record(1)), convert2BytesToShort(record(2), record(3)), record(4) == 0))
    dataRecords.toList
  }

  private def calculateShiftX(points: List[Point2D]) = {
    val minX = points.map(point => point.x).min
    if(minX < 0) -minX else 0
  }

  private def calculateShiftY(points: List[Point2D]) = {
    val minY = points.map(point => point.y).min
    if(minY < 0) -minY else 0
  }

  private def shiftToPositiveSquare(points: List[Point2D]) = {
    val shiftX = calculateShiftX(points)
    val shiftY = calculateShiftY(points)
    points.map(point => Point2D(point.x+shiftX, point.y+shiftY, point.laserOn))
  }

  private def writeGcodeOutput(onlyPositivePointsList: List[Point2D], scaleDivider: Int, outputGcodeFilePath: String) = {
    require(scaleDivider > 0, "scaleDivider must be positive int")
    val pw = new PrintWriter(new File(outputGcodeFilePath))
    var lastLaserOn = false;
    pw.println(LaserOffGcode)
    for (point2D <- onlyPositivePointsList) {
      val currentLaserOn = point2D.laserOn
      if (currentLaserOn != lastLaserOn) {
        if (currentLaserOn) {
          pw.println(LaserOnGcode)
        } else {
          pw.println(LaserOffGcode)
        }
      }
      lastLaserOn = currentLaserOn
      pw.println(s"X${point2D.x/scaleDivider} Y${point2D.y/scaleDivider}")
    }
    pw.close
  }
}

object Processor {
  val processor: IldaFileProcessor = new IldaFileProcessor()
  def processFile (inputIldaFilePath: String, outputGcodeFilePath: String, scaleDivider: Int) = processor.processIldaFile(inputIldaFilePath,
    outputGcodeFilePath, scaleDivider)
  def usage() = println("Processor.processFile(\"./ildatest_format1.ild\", \"./ildatest_f1.gcode\", 100)")
}

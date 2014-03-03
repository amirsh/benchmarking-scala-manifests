package scala.slick.benchmark

import org.scalameter.api._
import org.scalameter.reporting.DsvReporter
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe._

object TestMan extends PerformanceTest {
  lazy val executor = LocalExecutor(
    new Executor.Warmer.Default,
    Aggregator.min,
    new Measurer.Default)
  lazy val reporter = Reporter.Composite(LoggingReporter(), CsvReporter(','))
  lazy val persistor = Persistor.None
  val sizes = Gen.range("Size")(1000000, 2000000, 500000)

  val ranges = for {
    size <- sizes
  } yield 0 until size

  import TP._


  performance of "TypeParams" in {
    measure method "caseClass" in {
      {
        
        {
          using(ranges) curve ("Manifest1D") in { r =>
            for (i <- r) {
              AManifest[Int](i)
            }
          }
        }
        {
          using(ranges) curve ("Manifest2D1P") in { r =>
            for (i <- r) {
              AManifest[List[Int]](i)
            }
          }
        }
        {
          using(ranges) curve ("Manifest2D2P") in { r =>
            for (i <- r) {
              AManifest[Int => String](i)
            }
          }
        }
        {
          using(ranges) curve ("TypeTag1D") in { r =>
            for (i <- r) {
              ATypeTag[Int](i)
            }
          }
        }
        {
          using(ranges) curve ("TypeTag2D1P") in { r =>
            for (i <- r) {
              ATypeTag[List[Int]](i)
            }
          }
        }
        {
          using(ranges) curve ("TypeTag2D2P") in { r =>
            for (i <- r) {
              ATypeTag[Int => String](i)
            }
          }
        }
        {
          using(ranges) curve ("TP1D") in { r =>
            for (i <- r) {
              ATP(i)(IntTp)
            }
          }
        }
        {
          using(ranges) curve ("TP2D1P") in { r =>
            for (i <- r) {
              ATP(i)(listTp(IntTp))
            }
          }
        }
        {
          using(ranges) curve ("TP2D2P") in { r =>
            for (i <- r) {
              ATP[Int => String](i)(funcTp(IntTp, StringTp))
            }
          }
        }
        {
          using(ranges) curve ("Node1D") in { r =>
            for (i <- r) {
              ANode(i).withTpe(IntTp)
            }
          }
        }
        {
          using(ranges) curve ("Node2D1P") in { r =>
            for (i <- r) {
              ANode(i).withTpe(listTp(IntTp))
            }
          }
        }
        {
          using(ranges) curve ("Node2D2P") in { r =>
            for (i <- r) {
              ANode(i).withTpe(funcTp(IntTp, StringTp))
            }
          }
        }
        {
          using(ranges) curve ("Nothing") in { r =>
            for (i <- r) {
              ASimple(i)
            }
          }
        }
      }
    }
  }
  performance of "TypeParams" in {
    measure method "caseClassTransfer" in {
      {
        {
          using(ranges) curve ("ManifestTrImplVal") in { r =>
            for (i <- r) {
              val a = AManifest[Int => String](i)
              implicit val tp = a.tp
              // AManifest[(Int, String)](i)
              AManifest[Int => String](i)
            }
          }
        }
        {
          using(ranges) curve ("ManifestTrNothing") in { r =>
            for (i <- r) {
              val a = AManifest[Int => String](i)
              // implicit val tp = a.tp
              // AManifest[(Int, String)](i)
              AManifest[Int => String](i)
            }
          }
        }
        {
          using(ranges) curve ("ManifestTrManDefInvoke") in { r =>
            for (i <- r) {
              val a = AManifest[Int => String](i)
              implicit val tp = a.tp
              // AManifest[(Int, String)](i)
              AManifest[Int => String](i)(manifest[Int => String])
            }
          }
        }
        {
          using(ranges) curve ("ManifestTrDefDef") in { r =>
            for (i <- r) {
              def recreate[T: Manifest](v: AManifest[T]): AManifest[T] = 
                AManifest(v.i)
              val a = AManifest[Int => String](i)
              recreate(a)
            }
          }
        }
      }
    }
  }
}

case class AManifest[T: Manifest](i: Int) {
  val tp = implicitly[Manifest[T]]
}
case class ATypeTag[T: TypeTag](i: Int)
case class ASimple(i: Int)
case class ATP[T: TP](i: Int)
trait TP[T]

trait HTP[T, H[_]] extends TP[H[T]] {
  val underlying: TP[T]
}
object TP {
  implicit object IntTp extends TP[Int]
  implicit object StringTp extends TP[String]
  implicit def listTp[T: TP] = new HTP[T, List] { val underlying = implicitly[TP[T]]}
  implicit def funcTp[T: TP, S: TP] = new TP[T => S] {}
}

case class ANode(i: Int) {
  final var symbol: NodeSymbol = _
  @inline final def withTpe(tp: TP[_]): this.type = {
    if (symbol == null)
      symbol = NodeSymbol()
    symbol.tp = tp
    this
  }
}

case class NodeSymbol() {
  final var tp: TP[_] = _
}

case class CsvReporter(delimiter: Char) extends Reporter {
  import org.scalameter._
  import java.io._
  import java.util.Date
  import java.util.TimeZone
  import java.text.SimpleDateFormat
  import utils.Tree

  val sep = File.separator

  def report(result: CurveData, persistor: Persistor) {
  }

  def report(result: Tree[CurveData], persistor: Persistor) = {
    val currentDate = new Date
    val resultdir = initialContext.goe(Key.reports.resultDir, "results")

    def getHeadCurveData(tree: Tree[CurveData]): CurveData = {
      tree.items.headOption.getOrElse({
        tree.children.filter(t => getHeadCurveData(t) != null).headOption.map(getHeadCurveData).getOrElse(null)
      }
      )
    }

    val headCurveData = getHeadCurveData(result)

    new File(s"$resultdir").mkdir()
    val filename = s"$resultdir$sep${headCurveData.context.scope}.csv"

    def print() {
      var writer: PrintWriter = null
      try {
        writer = new PrintWriter(new FileWriter(filename, false))
        //        writer = System.out
        writeData(writer)
      } finally {
        if (writer != null) writer.close()
      }
    }

    def writeData(pw: PrintWriter) {
      var tabular = new ArrayBuffer[List[Any]]
      def header(cd: CurveData) = {
        //        "Method" + delimiter + cd.measurements.map(_.params.axisData.head._2).mkString(delimiter.toString)
        tabular += List("Method") ++ cd.measurements.map(_.params.axisData.head._2)
      }

      def row(cd: CurveData) = {
        //        cd.context.curve + delimiter + cd.measurements.map(m => m.value).mkString(delimiter.toString)
        tabular += List(cd.context.curve) ++ cd.measurements.map(_.value)
      }

      //      def header(cd: CurveData) = {
      //        val m = cd.measurements.head
      //        "Method" + delimiter + m.params.axisData.head._1 + delimiter + s"Time (${m.units})"
      //      }
      //
      //      def row(cd: CurveData) = {
      //        cd.measurements.map(m => cd.context.curve + delimiter + m.params.axisData.head._2 + delimiter + m.value).mkString("\n")
      //      }

      //      pw.println(header(headCurveData))
      //      result foreach { cd =>
      //        pw.println(row(cd))
      //
      //      }
      header(headCurveData)
      result foreach row
      tabular.toList.transpose foreach { line =>
        pw.println(line.mkString(delimiter.toString))
      }
    }

    print()

    true
  }

}

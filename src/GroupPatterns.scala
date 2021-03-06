import java.io.{File, FileWriter}
import java.text.SimpleDateFormat
import java.util.Date
import scala.io.Source

/**
  *
  */
object Category {
  //noinspection ScalaDeprecation
  def quarterIdx(d: Date): Int =
    d.getYear * 4 + Math.floor(d.getMonth / 3).toInt

  val format = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssX")


  def visualizeHistory(csvFilename: String) {
    println(s"processing $csvFilename")
    val lines = Source.fromFile(csvFilename).getLines().toSeq
    val commits: Seq[(String, Date)] = for (line <- lines) yield {
      val s = line.split(",")
      (s(1), format.parse(s(3)))

    }
    val firstCommit = commits.map(_._2).min
    val firstQuarter = quarterIdx(firstCommit)
    val lastCommit = commits.map(_._2).max

    val quarterCount = quarterIdx(lastCommit) - firstQuarter
    val commitsMonthIdx: Seq[(String, Int)] = commits.map(d => (d._1, quarterIdx(d._2) - firstQuarter))
    if (commits.filter(_._1 != "beforeForking").map(_._2).isEmpty) {
      println("fork is even with upstream")
      return
    }
    val forkTime2 = commits.filter(_._1 != "beforeForking").map(_._2).min
    val forkQuarter = quarterIdx(forkTime2) - firstQuarter

    def quarterLabel(qIdx: Int): String = {
      val year = ((qIdx + firstQuarter) / 4).toInt - 100
      val q = (qIdx + firstQuarter) % 4 + 1
      "'" + year + "-" + q
    }


    if (quarterLabel(forkQuarter) == "19-1" || quarterLabel(forkQuarter) == "19-2") {
      println("fork point too late")
      val repo = csvFilename.replace("_commit_date_category.csv", "").replace("analyzed_commitHistory", "")
      val w = new FileWriter("analyzed_commitHistory/tooLate.csv", true)
      w.write(repo + "\n")
      w.close()
      return
    }

    var U_ActiveBeforeForkingWithin_1_Qt = 0
    var U_ActiveBeforeForkingWithin_2_Qt = 0
    var U_ActiveBeforeForkingWithin_3_Qt = 0
    var U_ActiveBeforeForkingWithin_1_Yr = 0
    var U_ActiveBeforeForkingWithin_2_Yr = 0
    var U_ActiveWithin_1_Qt_afterForkingPoint = 0
    var U_ActiveWithin_1_Yr_afterForkingPoint = 0
    var U_ActiveWithin_2_Yr_afterForkingPoint = 0
    var U_ActiveWithin_3_Yr_afterForkingPoint = 0
    var U_ActiveAfter_3_Yr_afterForkingPoint = 0


    var F_ActiveWithin_1_Qt = 0
    var F_ActiveWithin_2_Qt = 0
    var F_ActiveWithin_3_Qt = 0
    var F_ActiveWithin_1_Yr = 0
    var F_ActiveWithin_2_Yr = 0
    var F_Active_After_2_Yr = 0


    var U_Active_TillTheEnd = 0
    var U_Active_Now = 0
    var F_Active_TillTheEnd = 0
    var F_Active_Now = 0

    var merge_count_within_1_Yr = 0
    var merge_count_within_2_Yr = 0
    var merge_count_after_2_Yr = 0

    var sync_count_within_1_Yr = 0
    var sync_count_within_2_Yr = 0
    var sync_count_after_2_Yr = 0


    val actionsPerQuarter = commitsMonthIdx.groupBy(x => x).mapValues(_.size)

    var sb = new StringBuffer()
    val dist = 50

    var forkCommit_total = 0


    var upstreamQtLatest = 0;
    var forkQtLatest = 0;
    var U2FQtLatest = 0;
    var F2UQtLatest = 0;

    for (quarterIdx <- 0 to quarterCount) {
      val mergeF2U = actionsPerQuarter.getOrElse(("F2U", quarterIdx), 0)
      val mergeU2F = actionsPerQuarter.getOrElse(("U2F", quarterIdx), 0)
      val upstreamCommits = actionsPerQuarter.getOrElse(("beforeForking", quarterIdx), 0) +
        actionsPerQuarter.getOrElse(("OnlyU", quarterIdx), 0) +
        mergeU2F
      val forkCommits = actionsPerQuarter.getOrElse(("OnlyF", quarterIdx), 0) +
        mergeF2U
      forkCommit_total += forkCommits

      val x = dist / 2 + dist * quarterIdx
      if (mergeF2U > 0) {
        F2UQtLatest = quarterIdx
        if (quarterIdx >= forkQuarter && quarterIdx < forkQuarter + 4) {
          merge_count_within_1_Yr += 1
        } else if (quarterIdx >= forkQuarter + 4 && quarterIdx < forkQuarter + 8) {
          merge_count_within_2_Yr += 1
        } else if (quarterIdx >= forkQuarter + 8) {
          merge_count_after_2_Yr += 1
        }
      }

      if (mergeU2F > 0) {
        U2FQtLatest = quarterIdx
        if (quarterIdx >= forkQuarter && quarterIdx < forkQuarter + 4) {
          sync_count_within_1_Yr += 1
        } else if (quarterIdx >= forkQuarter + 4 && quarterIdx < forkQuarter + 8) {
          sync_count_within_2_Yr += 1
        } else if (quarterIdx >= forkQuarter + 8) {
          sync_count_after_2_Yr += 1
        }
      }

      if (upstreamCommits > 0) {
        upstreamQtLatest = quarterIdx

        //       before forking point
        if (quarterIdx >= forkQuarter - 4 && quarterIdx < forkQuarter) {
          U_ActiveBeforeForkingWithin_1_Yr += 1
          if (quarterIdx == forkQuarter - 1) {
            U_ActiveBeforeForkingWithin_1_Qt += 1
          } else if (quarterIdx == forkQuarter - 2) {
            U_ActiveBeforeForkingWithin_2_Qt += 1
          } else if (quarterIdx == forkQuarter - 3) {
            U_ActiveBeforeForkingWithin_3_Qt += 1
          }
        } else if (quarterIdx >= forkQuarter - 8 && quarterIdx < forkQuarter - 4) {
          U_ActiveBeforeForkingWithin_2_Yr += 1
        }


        //        after forking point

        if (quarterIdx >= forkQuarter && quarterIdx < forkQuarter + 4) {
          U_ActiveWithin_1_Yr_afterForkingPoint += 1
          if (quarterIdx == forkQuarter) {
            U_ActiveWithin_1_Qt_afterForkingPoint += 1
          }
        } else if (quarterIdx >= forkQuarter + 4 && quarterIdx < forkQuarter + 8) {
          U_ActiveWithin_2_Yr_afterForkingPoint += 1
        } else if (quarterIdx >= forkQuarter + 8 && quarterIdx < forkQuarter + 16) {
          U_ActiveWithin_3_Yr_afterForkingPoint += 1
        } else if (quarterIdx >= forkQuarter + 16) {
          U_ActiveAfter_3_Yr_afterForkingPoint += 1
        }


        if (quarterIdx == quarterCount) {
          U_Active_TillTheEnd = 1
        }
      }

      if (forkCommits > 0) {
        forkQtLatest = quarterIdx
        if (quarterIdx == forkQuarter) {
          F_ActiveWithin_1_Qt += 1
        } else if (quarterIdx == forkQuarter + 1) {
          F_ActiveWithin_2_Qt += 1
        } else if (quarterIdx == forkQuarter + 2) {
          F_ActiveWithin_3_Qt += 1
        } else if (quarterIdx == forkQuarter + 3) {
          F_ActiveWithin_1_Yr += 1
        } else if (quarterIdx > forkQuarter + 3 && quarterIdx <= forkQuarter + 7) {
          F_ActiveWithin_2_Yr += 1
        } else if (quarterIdx > forkQuarter + 7) {
          F_Active_After_2_Yr += 1
        }

        if (quarterIdx == quarterCount) {
          F_Active_TillTheEnd = 1
        }
      }

    }


    var file_output = ""
    if (forkCommit_total == 0) {
      println("fork is inactive after forking point")
      return
    } else {
//      file_output = "/DATA/shurui/ForkData/analyzed_commitHistory/categoryResult.csv"
            file_output = "/Users/shuruiz/Work/ForkData/hardfork-exploration/EvolutionGraph/categoryResult.csv"
    }

    val diff = new Date().getTime - lastCommit.getTime
    val diffDays = diff / (24 * 60 * 60 * 1000)
    println("diff day " + diffDays)
    if (diffDays < 30) {
      if (F_Active_TillTheEnd == 1) F_Active_Now = 1
      if (U_Active_TillTheEnd == 1) U_Active_Now = 1
    }


    val repo = csvFilename.replace("_commit_date_category.csv", "").replace("/DATA/shurui/ForkData/analyzed_commitHistory/", "")
    sb.append(repo + "," + U_ActiveBeforeForkingWithin_1_Qt + "," + U_ActiveBeforeForkingWithin_2_Qt + "," + U_ActiveBeforeForkingWithin_3_Qt + "," + U_ActiveBeforeForkingWithin_1_Yr
      + "," + U_ActiveBeforeForkingWithin_2_Yr + "," + U_ActiveWithin_1_Qt_afterForkingPoint + "," + U_ActiveWithin_1_Yr_afterForkingPoint + "," + U_ActiveWithin_2_Yr_afterForkingPoint + ","
      + U_ActiveWithin_3_Yr_afterForkingPoint + "," + U_ActiveAfter_3_Yr_afterForkingPoint + "," + F_ActiveWithin_1_Qt + "," + F_ActiveWithin_2_Qt + "," + F_ActiveWithin_3_Qt + ","
      + F_ActiveWithin_1_Yr + "," + F_ActiveWithin_2_Yr + "," + F_Active_After_2_Yr + "," + U_Active_TillTheEnd + "," + U_Active_Now + "," + F_Active_TillTheEnd + "," + F_Active_Now + ","
      + merge_count_within_1_Yr + "," + merge_count_within_2_Yr + "," + merge_count_after_2_Yr + "," + sync_count_within_1_Yr + "," + sync_count_within_2_Yr + "," + sync_count_after_2_Yr + ","
      + quarterCount + "," + upstreamQtLatest + "," + forkQtLatest + "," + U2FQtLatest + "," + F2UQtLatest + "\n")
    val w = new FileWriter(file_output, true)
    w.write(sb.toString)
    w.close()


  }


  //args.foreach(visualizeHistory)
  def main(args: Array[String]): Unit = {

    visualizeHistory("/Users/shuruiz/Work/ForkData/hardfork-exploration/EvolutionGraph/sejoung.iBatisLogParser_commit_date_category.csv")
    //    visualizeHistory("/Users/shuruiz/Work/ForkData/hardfork-exploration/EvolutionGraph/Astron.panda3d_commit_date_category.csv")

  }
}
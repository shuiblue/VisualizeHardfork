import java.io.FileWriter
import java.nio.file.{Files, Paths, StandardCopyOption}
import java.text.SimpleDateFormat
import java.util.Date

import scala.io.Source

/**
  *
  */
object HelloWorld {
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
    //val forkTime = commits.filter(_._1 == "beforeForking").map(_._2).max
    if (commits.filter(_._1 != "beforeForking").map(_._2).isEmpty) {
      println("fork is even with upstream")
      return
    }
    val forkTime2 = commits.filter(_._1 != "beforeForking").map(_._2).min
    //if (forkTime.after(forkTime2))
    //  System.err.println(s"last beforeForking commit ($forkTime) is after first forked commit ($forkTime2)")
    val forkQuarter = quarterIdx(forkTime2) - firstQuarter

    def quarterLabel(qIdx: Int): String = {
      val year = ((qIdx + firstQuarter) / 4).toInt - 100
      val q = (qIdx + firstQuarter) % 4 + 1
      "'" + year + "-" + q
    }

    val actionsPerQuarter = commitsMonthIdx.groupBy(x => x).mapValues(_.size)

    var svg = new StringBuffer()

    val dist = 50

    svg.append(
      s"""<svg version="1.2" xmlns="http://www.w3.org/2000/svg"
         |xmlns:xlink="http://www.w3.org/1999/xlink" class="graph"
         |aria-labelledby="title" role="img"
         |height="${3 * dist}" width="${quarterCount * dist + dist}">
         |  <defs>
    <marker id="arrow" markerWidth="10" markerHeight="10" refX="8" refY="3" orient="auto" markerUnits="userSpaceOnUse">
      <path d="M0,0 L0,6 L9,3 z" fill="#f00" stroke-width="0"/>
    </marker>
  </defs>""".stripMargin)
    svg.append("""<g class="data" data-setname="">""")

    svg.append(s"""<line x1="${dist / 2}" x2="${dist / 2 + dist * quarterCount}" y1="${dist / 2}" y2="${dist / 2}" stroke="orange" stroke-width="2"/>""")
    svg.append(s"""<line x1="${dist / 2 + forkQuarter * dist}" x2="${dist / 2 + dist * quarterCount}" y1="${dist * 1.5}" y2="${dist * 1.5}" stroke="blue" stroke-width="2"/>""")

    var forkingQt_x1 = -dist / 2 + forkQuarter * dist
    if(forkingQt_x1<0){
      forkingQt_x1 = dist / 2 + dist * forkQuarter
    }


//    svg.append(s"""<line x1="${-dist / 2 + forkQuarter * dist}" x2="${dist / 2 + dist * forkQuarter}" y1="${dist * 0.5}" y2="${dist * 1.5}" stroke="blue" stroke-width="2"/>""")
    svg.append(s"""<line x1="${forkingQt_x1}" x2="${dist / 2 + dist * forkQuarter}" y1="${dist * 0.5}" y2="${dist * 1.5}" stroke="blue" stroke-width="2"/>""")


    var forkCommit_total = 0

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
      if (mergeF2U > 0)
        svg.append(s"""<path d="M$x ${dist * 1.5} C ${x -.2 * dist} ${dist * 1.3}, ${x -.2 * dist} ${dist * .8}, $x ${dist * .5}" stroke="orange" stroke-width="${1 + Math.log(mergeF2U) / 2}" fill="transparent" marker-end="url(#arrow)"/>""")
      if (mergeU2F > 0)
        svg.append(s"""<path d="M$x ${dist * .5} C ${x +.2 * dist} ${dist * .8}, ${x +.2 * dist} ${dist * 1.3}, $x ${dist * 1.5}" stroke="blue" stroke-width="${1 + Math.log(mergeU2F) / 2}" fill="transparent" marker-end="url(#arrow)"/>""")
      if (upstreamCommits > 0)
        svg.append(s"""<circle cy="${dist / 2}" cx="$x" data-value="7.2" r="${3 + 2 * Math.log(upstreamCommits)}"></circle>""")
      if (forkCommits > 0)
        svg.append(s"""<circle cy="${dist * 1.5}" cx="$x" data-value="7.2" r="${3 + 2 * Math.log(forkCommits)}"></circle>""")

      svg.append(s"""<text x="$x" y="${dist * 2.5}" font-size="18" text-anchor="middle">${quarterLabel(quarterIdx)}</text>""")

      //    println(s"$quarterIdx: $upstreamCommits\t<-$mergeF2U,$mergeU2F->\t$forkCommits" + (if (forkQuarter == quarterIdx) "**" else ""))
    }

    var title = """"<text x="15" y="15">"""" + csvFilename.replace("_commit_date_category.csv", "") +""""</text>""""

    svg.append(title)

    svg.append("""</g></svg>""")

    var file_output = ""
    if (forkCommit_total == 0) {
      println("fork is inactive after forking point")
      return
    }else {
//      file_output = csvFilename + ".svg"
      file_output ="/Users/shuruiz/Work/ForkData/test/"+csvFilename.replace("/Users/shuruiz/Work/ForkData/hardfork-exploration/EvolutionGraph","") + ".svg"
    }
      val w = new FileWriter(file_output)
      w.write(svg.toString)
      w.close()






    val path = Files.move(
      Paths.get(csvFilename),
      Paths.get("/Users/shuruiz/Work/ForkData/test/"+csvFilename.replace("/Users/shuruiz/Work/ForkData/hardfork-exploration/EvolutionGraph","")),
      StandardCopyOption.REPLACE_EXISTING
    )

    if (path != null) {
      println(s"moved the file $csvFilename successfully")
    } else {
      println(s"could NOT move the file $csvFilename")
    }

  }

  //args.foreach(visualizeHistory)
  def main(args: Array[String]): Unit = {
    //    println("Hello, world!")
    visualizeHistory("/Users/shuruiz/Work/ForkData/hardfork-exploration/EvolutionGraph/badjer.Lasy_commit_date_category.csv")
    //    visualizeHistory("/Users/shuruiz/Work/ForkData/hardfork-exploration/EvolutionGraph/Astron.panda3d_commit_date_category.csv")

  }
}
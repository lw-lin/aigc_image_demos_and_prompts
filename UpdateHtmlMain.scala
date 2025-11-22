import java.io.{File, PrintWriter}
import java.nio.file.{Files, Paths}
import scala.io.Source
import scala.util.{Try, Success, Failure}
import scala.util.control.Breaks._

/**
   * 我们对 details 目录里的数据做如下设计：
   * 1、details 目录里，只能有子文件夹（记为 1 级子目录）、不能有子文件；每个 1 级子目录里，只能有子文件、不能有子文件夹
   * 2、1 级子目录里，必须有 prompts_and_details.md 文件；且文件内容里，必须且只能有：source、prompt、extra_info 这 3 个 #一级标题
   * 3、1 级子目录里，必须有一个图片文件，它的名字是：img.jpg 或 img.png
   * 4、1 级子目录里，如果有其它文件，必须以 extra_ 开头
   */
object UpdateHtmlMain {
  val detailsDir = new File("details")
  val indexHtmlPath = "index.html"
  
  // 支持的图片扩展名
  val imageExtensions = Set(".jpg", ".jpeg", ".png", ".gif", ".webp")
  

  def main(args: Array[String]): Unit = {
    println("开始生成 tableData...")
    checkDataDir()
    updateIndexHtml()
    println("完成！")
  }

  /**
   * 按照我们对 details 目录里的数据设计，检查数据是否合法
   */
  def checkDataDir(): Unit = {
    if (!detailsDir.exists() || !detailsDir.isDirectory) {
      throw new RuntimeException("错误：details 目录不存在")
    }
    
    var hasError = false
    
    // 检查 1 级子目录
    val level1Dirs = detailsDir.listFiles().filter(_.isDirectory).toList
    val level1Files = detailsDir.listFiles().filter(_.isFile).toList
    
    // 1. 检查 details 目录下不能有文件
    if (level1Files.nonEmpty) {
      println(s"错误：details 目录下不能有文件，但发现了：${level1Files.map(_.getName).mkString(", ")}")
      hasError = true
    }
    
    // 遍历每个 1 级子目录
    level1Dirs.foreach { level1Dir =>
      val level1Name = level1Dir.getName
      
      // 检查 1 级子目录下不能有子文件夹
      val level1SubDirs = level1Dir.listFiles().filter(_.isDirectory).toList
      if (level1SubDirs.nonEmpty) {
        println(s"错误：1 级子目录 '$level1Name' 下不能有子文件夹，但发现了：${level1SubDirs.map(_.getName).mkString(", ")}")
        hasError = true
      }
      
      // 获取 1 级子目录下的所有文件
      val allFiles = level1Dir.listFiles().filter(_.isFile).toList
      
      // 2. 检查 prompts_and_details.md 文件
      val mdFile = new File(level1Dir, "prompts_and_details.md")
      if (!mdFile.exists()) {
        println(s"错误：1 级子目录 '$level1Name' 中缺少 prompts_and_details.md 文件")
        hasError = true
      } else {
        // 检查 markdown 文件内容
        checkMarkdownContent(mdFile, level1Name) match {
          case Some(error) =>
            println(error)
            hasError = true
          case None => // 检查通过
        }
      }
      
      // 3. 检查图片文件（img.jpg 或 img.png）
      val imgJpg = new File(level1Dir, "img.jpg")
      val imgPng = new File(level1Dir, "img.png")
      if (!imgJpg.exists() && !imgPng.exists()) {
        println(s"错误：1 级子目录 '$level1Name' 中缺少 img.jpg 或 img.png 文件")
        hasError = true
      }
      
      // 4. 检查其他文件是否以 extra_ 开头
      val requiredFiles = Set("prompts_and_details.md", "img.jpg", "img.png")
      val otherFiles = allFiles.filter(f => !requiredFiles.contains(f.getName))
      
      otherFiles.foreach { file =>
        if (!file.getName.startsWith("extra_")) {
          println(s"错误：1 级子目录 '$level1Name' 中的文件 '${file.getName}' 必须以 extra_ 开头")
          hasError = true
        }
      }
    }
    
    if (hasError) {
      throw new RuntimeException("数据目录检查失败，请修复上述错误后重试")
    } else {
      println("数据目录检查通过")
    }
  }
  
  // 检查 markdown 文件内容是否包含且仅包含 source、prompt、extra_info 三个一级标题
  def checkMarkdownContent(mdFile: File, dirPath: String): Option[String] = {
    Try {
      val content = Source.fromFile(mdFile, "UTF-8").mkString
      val lines = content.split("\n").toList
      
      // 查找所有一级标题（以 # 开头，后面跟空格，然后是标题文本）
      val h1Pattern = """^#\s+(.+)$""".r
      val h1Titles = lines.collect {
        case h1Pattern(title) => title.trim.toLowerCase
      }
      
      val requiredTitles = Set("source", "prompt", "extra_info")
      val foundTitles = h1Titles.toSet
      
      // 检查是否包含所有必需的标题
      val missingTitles = requiredTitles -- foundTitles
      if (missingTitles.nonEmpty) {
        Some(s"错误：1 级子目录 '$dirPath' 的 prompts_and_details.md 中缺少一级标题：${missingTitles.mkString(", ")}")
      } else {
        // 检查是否只有这三个标题（允许重复，但只允许这三个）
        val extraTitles = foundTitles -- requiredTitles
        if (extraTitles.nonEmpty) {
          Some(s"错误：1 级子目录 '$dirPath' 的 prompts_and_details.md 中只能有 source、prompt、extra_info 三个一级标题，但发现了：${extraTitles.mkString(", ")}")
        } else {
          None
        }
      }
    } match {
      case Success(result) => result
      case Failure(e) => Some(s"错误：读取 1 级子目录 '$dirPath' 的 prompts_and_details.md 文件失败：${e.getMessage}")
    }
  }

  // 从 markdown 文件中提取标题和详情
  def extractTitleAndDetails(mdFile: File): (String, String) = {
    if (!mdFile.exists()) {
      return ("标题", "details")
    }
    
    Try {
      val content = Source.fromFile(mdFile, "UTF-8").mkString
      val lines = content.split("\n").toList
      
      // 尝试提取标题：查找包含"标题："的行
      val title = lines.find(_.contains("标题：")) match {
        case Some(line) => 
          val index = line.indexOf("标题：")
          if (index >= 0) {
            val titlePart = line.substring(index + "标题：".length).trim
            if (titlePart.nonEmpty) titlePart else "标题"
          } else {
            "标题"
          }
        case None => 
          // 如果没有找到"标题："，尝试从 prompt 部分提取
          val promptIndex = lines.indexWhere(_.contains("# prompt"))
          if (promptIndex >= 0 && promptIndex + 1 < lines.length) {
            val promptLine = lines(promptIndex + 1).trim
            if (promptLine.nonEmpty && !promptLine.startsWith("#")) {
              promptLine.take(50) // 限制长度
            } else {
              "标题"
            }
          } else {
            "标题"
          }
      }
      
      // details 使用整个文件内容，但限制长度
      val details = if (content.length > 500) {
        content.take(500) + "..."
      } else {
        content
      }
      
      (title, details)
    } match {
      case Success(result) => result
      case Failure(_) => ("标题", "details")
    }
  }
  
  // 查找目录中的图片文件
  def findImageFile(dir: File): Option[File] = {
    if (!dir.exists() || !dir.isDirectory) {
      return None
    }
    
    dir.listFiles()
      .find { file =>
        file.isFile && imageExtensions.exists { ext =>
          file.getName.toLowerCase.endsWith(ext)
        }
      }
  }
  
  // 生成 tableData JSON
  def generateTableData(): String = {
    if (!detailsDir.exists() || !detailsDir.isDirectory) {
      return """{
            "rows": []
        }"""
    }
    
    val rows = detailsDir.listFiles()
      .filter(_.isDirectory)
      .sortBy(_.getName)
      .map { level1Dir =>
        val category = level1Dir.getName
        
        // 查找 1 级目录下的图片和 markdown
        val level1Image = findImageFile(level1Dir)
        val level1Md = new File(level1Dir, "prompts_and_details.md")
        val (level1Title, level1Details) = extractTitleAndDetails(level1Md)
        
        // 收集所有 cells（1 级目录下的图片）
        val cells = scala.collection.mutable.ListBuffer[Map[String, Any]]()
        
        // 如果 1 级目录有图片，添加到 cells
        level1Image.foreach { img =>
          val relativePath = s"details/${category}/${img.getName}"
          cells += Map(
            "image" -> relativePath,
            "tooltip" -> Map(
              "title" -> level1Title,
              "details" -> level1Details
            )
          )
        }
        
        Map(
          "category" -> category,
          "cells" -> cells.toList
        )
      }
      .filter(row => row("cells").asInstanceOf[List[?]].nonEmpty) // 只保留有 cells 的行
      .toList
    
    // 转换为 JSON 字符串
    val rowsJson = rows.map { row =>
      val category = row("category").asInstanceOf[String]
      val cells = row("cells").asInstanceOf[List[Map[String, Any]]]
      
      val cellsJson = cells.map { cell =>
        val image = cell("image").asInstanceOf[String]
        val tooltip = cell("tooltip").asInstanceOf[Map[String, String]]
        val title = tooltip("title")
        val details = tooltip("details")
        
        s"""                        { "image": ${escapeJson(image)}, "tooltip": { "title": ${escapeJson(title)}, "details": ${escapeJson(details)} } }"""
      }.mkString(",\n")
      
      s"""                {
                    "category": "$category",
                    "cells": [
$cellsJson
                    ]
                }"""
    }.mkString(",\n")
    
    s"""        const tableData = {
            "rows": [
$rowsJson
            ]
        };"""
  }
  
  // 转义 JSON 字符串（注意顺序：先转义反斜杠）
  def escapeJson(str: String): String = {
    "\"" + str
      .replace("\\", "\\\\")  // 先转义反斜杠
      .replace("\"", "\\\"")  // 再转义引号
      .replace("\n", "\\n")
      .replace("\r", "\\r")
      .replace("\t", "\\t")
      + "\""
  }
  
  // 更新 index.html
  def updateIndexHtml(): Unit = {
    val htmlContent = Source.fromFile(indexHtmlPath, "UTF-8").mkString
    
    // 查找 tableData 的开始和结束位置
    val startPattern = "const tableData = {"
    
    val startIndex = htmlContent.indexOf(startPattern)
    if (startIndex == -1) {
      println("错误：在 index.html 中找不到 tableData 的开始位置")
      return
    }
    
    // 找到对应的结束位置（查找匹配的 };）
    var braceCount = 0
    var foundStart = false
    var endIndex = startIndex
    
    breakable {
      for (i <- startIndex until htmlContent.length) {
        val char = htmlContent(i)
        if (char == '{') {
          braceCount += 1
          foundStart = true
        } else if (char == '}') {
          braceCount -= 1
          if (foundStart && braceCount == 0) {
            // 检查后面是否有分号
            var j = i + 1
            while (j < htmlContent.length && htmlContent(j).isWhitespace) {
              j += 1
            }
            if (j < htmlContent.length && htmlContent(j) == ';') {
              endIndex = j + 1
              break
            }
          }
        }
      }
    }
    
    if (endIndex == startIndex) {
      println("错误：在 index.html 中找不到 tableData 的结束位置")
      return
    }
    
    val newTableData = generateTableData()
    val updatedContent = htmlContent.substring(0, startIndex) + 
                        newTableData + 
                        htmlContent.substring(endIndex)
    
    // 写入文件
    val writer = new PrintWriter(indexHtmlPath, "UTF-8")
    try {
      writer.write(updatedContent)
      println(s"成功更新 $indexHtmlPath")
    } finally {
      writer.close()
    }
  }
}

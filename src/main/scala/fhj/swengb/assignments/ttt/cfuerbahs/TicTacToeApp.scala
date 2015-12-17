package fhj.swengb.assignments.ttt.cfuerbahs

import java.net.URL
import java.util.ResourceBundle
import javafx.application.Application
import javafx.fxml.{FXML, FXMLLoader, Initializable}
import javafx.scene.control.Button
import javafx.scene.image.{Image, ImageView}
import javafx.scene.layout.BorderPane
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.event.ActionEvent
import scala.util.control.NonFatal

object TicTacToeApp {
  def main(args: Array[String]) {
    Application.launch(classOf[TicTacToeApp], args: _*)
  }
}

class TicTacToeApp extends javafx.application.Application {


  val Fxml = "/fhj/swengb/assignments/ttt/cfuerbahs/TicTacToeApp.fxml"
  //val Css = ""

  val loader = new FXMLLoader(getClass.getResource(Fxml))

  override def start(stage: Stage): Unit =
    try {
      stage.setTitle("TicTacToeApp")
      loader.load[Parent]() // side effect
      val scene = new Scene(loader.getRoot[Parent])
      stage.setScene(scene)
      //stage.getScene.getStylesheets.add(Css)
      stage.show()
    } catch {
      case NonFatal(e) => e.printStackTrace()
    }

}


class TicTakToeController extends Initializable {
  @FXML var borderPane: BorderPane = _

  override def initialize(location: URL, resources: ResourceBundle): Unit = {

  }

  val movesMap = Map(
    ("btnTopLeft", TopLeft),
    ("btnTopCenter", TopCenter),
    ("btnTopRight", TopRight),

    ("btnMiddleLeft", MiddleLeft),
    ("btnMiddleCenter", MiddleCenter),
    ("btnMiddleRight", MiddleRight),

    ("btnBottomLeft", BottomLeft),
    ("btnBottomCenter", BottomCenter),
    ("btnBottomRight", BottomRight)

  )


  def execMove(evt: ActionEvent): Unit = {
    //println(evt.getSource.asInstanceOf[Button].getId + " -> " + movesMap(evt.getSource.asInstanceOf[Button].getId))
    println(evt.getSource.asInstanceOf[Button].setText("clicked") + " -> " + movesMap(evt.getSource.asInstanceOf[Button].getId))
  }


}
module Main where

import           Protolude                    hiding (Selector)
import           GHC.IO.Handle
import           System.Process
import           Test.WebDriver
import           Test.WebDriver.Commands
import           Test.WebDriver.Commands.Wait
import           Test.WebDriver.JSON


doExpectNItems :: Int -> WD [Element] -> WD [Element]
doExpectNItems n act = do
  items <- act
  expect $ length items == n
  return items

todoLabel :: Element -> WD Text
todoLabel el = do
  labelEl <- findElemFrom el $ ByCSS "label"
  getText labelEl

clearLocalStorage :: WD ()
clearLocalStorage = do
  ignoreReturn $ executeJS [] "localStorage.clear();"
  refresh

playTodo :: WD ()
playTodo = do
  openPage "http://todomvc.com/examples/vue"

  -- phantomJS does not support the localeStorage api it seems...
  clearLocalStorage

  title <- getTitle
  putStrLn title

  -- wait for the todo input
  todoInput <- waitUntil 5 $ findElem $ ByCSS "input.new-todo"

  sendKeys "try PhantomJS\n" todoInput
  sendKeys "try some more\n" todoInput

  todoList <- findElem $ ByCSS "ul.todo-list"

  let findListItems = findElemsFrom todoList $ ByCSS "li.todo"
  todoItems <- waitUntil 5 $ doExpectNItems 2 findListItems

  labels <- traverse todoLabel todoItems

  traverse_ putStrLn labels

  return ()


config :: WDConfig
config = defaultConfig

main :: IO ()
main = do
  (_, Just hout, _, phdl) <- createProcess (proc "phantomjs" ["--webdriver=localhost:4444"]) { std_out = CreatePipe }

  -- wait for phantom to output its first line
  _ <- hGetLine hout

  finally
    (runSession config . finallyClose $ playTodo)
    (do
      interruptProcessGroupOf phdl
      waitForProcess phdl
      )

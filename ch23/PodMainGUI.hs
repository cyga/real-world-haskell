-- file: ch23/PodMainGUI.hs
add dbh url = 
    do addPodcast dbh pc
       commit dbh
    where pc = Podcast {castId = 0, castURL = url}

update :: IConnection conn => conn -> (String -> IO ()) -> IO ()
update dbh logf = 
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
       logf "Update complete."
    where procPodcast pc =
              do logf $ "Updating from " ++ (castURL pc)
                 updatePodcastFromFeed dbh pc

download dbh logf =
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
       logf "Download complete."
    where procPodcast pc =
              do logf $ "Considering " ++ (castURL pc)
                 episodelist <- getPodcastEpisodes dbh pc
                 let dleps = filter (\ep -> epDone ep == False)
                             episodelist
                 mapM_ procEpisode dleps
          procEpisode ep =
              do logf $ "Downloading " ++ (epURL ep)
                 getEpisode dbh ep-- file: ch23/PodMainGUI.hs
guiUpdate :: IConnection conn => GUI -> conn -> IO ()
guiUpdate gui dbh = 
    statusWindow gui dbh "Pod: Update" (update dbh)

guiDownload gui dbh =
    statusWindow gui dbh "Pod: Download" (download dbh)

guiFetch gui dbh =
    statusWindow gui dbh "Pod: Fetch" 
                     (\logf -> update dbh logf >> download dbh logf)-- file: ch23/PodMainGUI.hs
statusWindow :: IConnection conn =>
                GUI 
             -> conn 
             -> String 
             -> ((String -> IO ()) -> IO ())
             -> IO ()
statusWindow gui dbh title func =
    do -- Clear the status text
       labelSetText (swLabel gui) ""
       
       -- Disable the OK button, enable Cancel button
       widgetSetSensitivity (swOKBt gui) False
       widgetSetSensitivity (swCancelBt gui) True

       -- Set the title
       windowSetTitle (statusWin gui) title

       -- Start the operation
       childThread <- forkIO childTasks

       -- Define what happens when clicking on Cancel
       onClicked (swCancelBt gui) (cancelChild childThread)
       
       -- Show the window
       windowPresent (statusWin gui)
    where childTasks =
              do updateLabel "Starting thread..."
                 func updateLabel
                 -- After the child task finishes, enable OK
                 -- and disable Cancel
                 enableOK
                 
          enableOK = 
              do widgetSetSensitivity (swCancelBt gui) False
                 widgetSetSensitivity (swOKBt gui) True
                 onClicked (swOKBt gui) (widgetHide (statusWin gui))
                 return ()

          updateLabel text =
              labelSetText (swLabel gui) text
          cancelChild childThread =
              do killThread childThread
                 yield
                 updateLabel "Action has been cancelled."
                 enableOK-- file: ch23/PodMainGUI.hs
guiAdd gui dbh = 
    do -- Initialize the add URL window
       entrySetText (awEntry gui) ""
       onClicked (awCancelBt gui) (widgetHide (addWin gui))
       onClicked (awOKBt gui) procOK
       
       -- Show the add URL window
       windowPresent (addWin gui)
    where procOK =
              do url <- entryGetText (awEntry gui)
                 widgetHide (addWin gui) -- Remove the dialog
                 add dbh url             -- Add to the DB-- file: ch23/PodMainGUI.hs
connectGui gui dbh =
    do -- When the close button is clicked, terminate GUI loop
       -- by calling GTK mainQuit function
       onDestroy (mainWin gui) mainQuit
       
       -- Main window buttons
       onClicked (mwAddBt gui) (guiAdd gui dbh)
       onClicked (mwUpdateBt gui) (guiUpdate gui dbh)
       onClicked (mwDownloadBt gui) (guiDownload gui dbh)
       onClicked (mwFetchBt gui) (guiFetch gui dbh)
       onClicked (mwExitBt gui) mainQuit

       -- We leave the status window buttons for later-- file: ch23/PodMainGUI.hs
loadGlade gladepath =
    do -- Load XML from glade path.
       -- Note: crashes with a runtime error on console if fails!
       Just xml <- xmlNew gladepath

       -- Load main window
       mw <- xmlGetWidget xml castToWindow "mainWindow"

       -- Load all buttons

       [mwAdd, mwUpdate, mwDownload, mwFetch, mwExit, swOK, swCancel,
        auOK, auCancel] <-
           mapM (xmlGetWidget xml castToButton)
           ["addButton", "updateButton", "downloadButton",
            "fetchButton", "exitButton", "okButton", "cancelButton",
            "auOK", "auCancel"]
       
       sw <- xmlGetWidget xml castToDialog "statusDialog"
       swl <- xmlGetWidget xml castToLabel "statusLabel"

       au <- xmlGetWidget xml castToDialog "addDialog"
       aue <- xmlGetWidget xml castToEntry "auEntry"

       return $ GUI mw mwAdd mwUpdate mwDownload mwFetch mwExit
              sw swOK swCancel swl au auOK auCancel aue-- file: ch23/PodMainGUI.hs
main :: FilePath -> IO ()
main gladepath = withSocketsDo $ handleSqlError $
    do initGUI                  -- Initialize GTK+ engine

       -- Every so often, we try to run other threads.
       timeoutAddFull (yield >> return True)
                      priorityDefaultIdle 100

       -- Load the GUI from the Glade file
       gui <- loadGlade gladepath

       -- Connect to the database
       dbh <- connect "pod.db"

       -- Set up our events 
       connectGui gui dbh

       -- Run the GTK+ main loop; exits after GUI is done
       mainGUI
       
       -- Disconnect from the database at the end
       disconnect dbh-- file: ch23/PodMainGUI.hs
-- | Our main GUI type
data GUI = GUI {
      mainWin :: Window,
      mwAddBt :: Button,
      mwUpdateBt :: Button,
      mwDownloadBt :: Button,
      mwFetchBt :: Button,
      mwExitBt :: Button,
      statusWin :: Dialog,
      swOKBt :: Button,
      swCancelBt :: Button,
      swLabel :: Label,
      addWin :: Dialog,
      awOKBt :: Button,
      awCancelBt :: Button,
      awEntry :: Entry}-- file: ch23/PodMainGUI.hs
module PodMainGUI where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

-- GUI libraries

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

-- Threading

import Control.Concurrent

import Control.Monad(when)
import System.Console.GetOpt
import System.Environment
import System.IO
import System.Posix.Signals

-- enumeration for the available command line options:
data OptionFlag = Append | IgnoreInterrupt
		  deriving (Show, Eq)

-- function to parse the command line, returning the options and other arguments (files):
parseCommand :: [String] -> IO ([OptionFlag], [String])
parseCommand args = 
    case getOpt Permute options args of
	(o,n,[]  ) -> return (o,n)		-- the normal case (error array is empty)
	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header  = "Usage: tee [-ai] files..."
	  options :: [ OptDescr OptionFlag ]
    	  options = [ Option ['a'] ["append"] (NoArg Append)          "append instead of overwrite"
	  	    , Option ['i'] ["ignore"] (NoArg IgnoreInterrupt) "ignore user interrupt"
	  	    ]

main = do
    args <- getArgs				-- bind command arguments to args
    (opts, files) <- parseCommand args		-- parse out the options from the files
    when (IgnoreInterrupt `elem` opts) $ do	-- should we disable interrupt?
        installHandler sigINT Ignore Nothing	-- ignore sigINT
	return ()				-- this returns an (IO ()) from the 'when'
    theOutput <- getContents			-- lazy String acts as an input pipe
    let mode = if (Append `elem` opts) then AppendMode else WriteMode	-- open mode based on -a
        in do
	    handles <- mapM (`openFile` mode) files			-- open all the files
	    					-- build a sequence of IO operations that output
						-- each line of text to each handle, starting
						-- with stdout
	    sequence [ hPutStrLn h l | l <- lines theOutput, h <- stdout:handles ]
            mapM_ hClose handles		-- close all the handles (except stdout)

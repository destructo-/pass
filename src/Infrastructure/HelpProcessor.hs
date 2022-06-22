module Infrastructure.HelpProcessor (
    process
) where

import qualified Services.Interaction as Interaction


process :: IO ()
process = Interaction.showHelp
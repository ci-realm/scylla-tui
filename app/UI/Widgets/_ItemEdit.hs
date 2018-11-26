module UI.Widgets.ItemEdit where

require Item
require Data.Text
require Data.Vector

import Data.Acid
import Data.Monoid
import Data.Default.Class
import Brick
import Brick.Types
import Brick.Forms
import Brick.Widgets.Border
import Brick.Widgets.Core
import Brick.Widgets.List
import Brick.Widgets.Center
import UI.Events (AppEvent)
import UI.Names
import TH (makeSuffixLenses)
import Lens.Micro --((^.))
import qualified Graphics.Vty as V

data ItemEditState = ItemEditState
  { _edited :: Maybe Int
  , _form :: Form Item AppEvent Name
  }

makeSuffixLenses ''ItemEditState

mkState ::ItemEditState
mkState = ItemEditState
  { _edited = Nothing
  , _form = itemForm def }

itemForm :: Item -> Form Item e Name
itemForm = newForm
  [ (str "Name: " <+>) @@=
      editTextField Item.name NameField (Just 1)
  , (str "EAN: " <+>) @@=
      editTextField Item.ean EANField (Just 1)
  , (str "Quantity: " <+>) @@=
      editShowableField Item.qty QTYField
  ]

mkWidget :: ItemEditState -> [Widget Name]
mkWidget s = [ui]
    where
        ui = vCenter $ vBox [ str $ show $ s^.editedL , renderForm $ s^.formL ]

handleEvent :: BrickEvent Name AppEvent -> ItemEditState -> EventM Name ItemEditState
handleEvent e@(VtyEvent ev) s = case e of
  _ -> do
    f' <- handleFormEvent e (s^.formL)
    return $ s & formL .~ f'

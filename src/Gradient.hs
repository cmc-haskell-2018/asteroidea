{-|
Module      : Gradient
Description : operations with gradients, generating invocation to Model
Copyright   : Just Nothing
Stability   : OLEG
-}
{--
Для базы необходимо лишь нормальное представление градиента и доступ к цветам в нем по Double [0,1]
Далее понадобятся функции для циклического сдвига, изменения hue\saturation\brightness и прочие ништяки
--}

--type Gradient = 
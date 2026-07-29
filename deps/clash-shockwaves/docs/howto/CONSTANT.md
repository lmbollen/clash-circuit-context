## How to implement Waveform for constant value types

Sometimes, it's easiest to just assign a constant value to a signal. This might be the case if you have a unit type, or
if actual translation is both difficult and not really necessary.

It is very simple to add a constant translator. Just create a `TConst` translator with the translation;
any subsignal hierarchy will be inferred automatically:

```hs
import Clash.Shockwaves.Waveform
myTranslator = TConst $
  Translation (Just ("value",WSNormal,11))
    [("subsignal",
      Translation (Just ("sub",WSNormal,11)) [] )]
```

However, it is still a bit tedious to implement full `Waveform` instances this way.
Instead, there is a much easier method:
the `WaveformConst` class.

Say you only need a single signal, like `()`. In that case, you only need to provide the `Render` value by overwriting `constRen`:


```hs
import Clash.Shockwaves.Waveform
instance WaveformConst () where
  constRen = Just ("()",WSNormal,11)
deriving via WaveformForConst () instance Waveform ()
```

Should you need a full translation, you should overwrite `constTrans` instead. The subsignal structure is automatically inferred from
this translation:

```hs
import Clash.Shockwaves.Waveform
instance WaveformConst MyType where
  constTrans = <translation>
deriving via WaveformForConst MyType instance Waveform MyType
```



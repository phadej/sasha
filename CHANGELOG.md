## 0.2

- Change the API such that

   ```diff
   - type Sasha tag = [(Tag, ERE)]
   + type Sasha r   = [(ERE, BS.ByteString -> BS.ByteString -> r)]
   ```

   This allows to write scanner actions more like in `alex`:

    ```haskell
    [ someRegexp := \tok inp' -> ...
    , another    := \tok inp' -> ...
    ]
    ```

    Similar change is also done in TTH interface.

- Add `charSet :: Word8Set -> ERE` helper.

## 0.1

- Use `word8set` package.

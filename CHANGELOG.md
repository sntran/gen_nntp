# CHANGELOG

## 0.18.0 (2021-03-12)

### Features

* "LAST" command ([f0e2d4a](https://github.com/sntran/gen_nntp/commit/f0e2d4a22155185753a2c920ea1d9da5c83be0bf))
* `[@callback](https://github.com/callback) handle_CAPABILITIES/1` ([4a12906](https://github.com/sntran/gen_nntp/commit/4a129068348f61864ba9cf2d7ee9c74d25577cfc))
* `connect/3` to connect to NNTP server ([a5b850a](https://github.com/sntran/gen_nntp/commit/a5b850a4a76ff44c6d3e79151b60673020aa0b7b))
* `handle_ARTICLE/2` callback ([10bf398](https://github.com/sntran/gen_nntp/commit/10bf398ac0307ce1ba7e1180a6b829f4f7421e48))
* `handle_GROUP` ([87bd933](https://github.com/sntran/gen_nntp/commit/87bd933be169dc2e8c04f968d1f4328f41895527))
* `HELP` command ([ceaa051](https://github.com/sntran/gen_nntp/commit/ceaa051bbc4af37cd539ab0ce901b9e021870f0d))
* all cases of ARTICLE and GROUP ([1587252](https://github.com/sntran/gen_nntp/commit/15872523505a5362150a1dd2fca588a1084a44a5))
* command with multi-line response ([8bd780f](https://github.com/sntran/gen_nntp/commit/8bd780f56a1fe15da7625820c30833fe21a88cdb))
* handle QUIT command ([295e545](https://github.com/sntran/gen_nntp/commit/295e545dcaa73d430581138fe5689a3665bbc156))
* handle VERSION capability ([484a8b6](https://github.com/sntran/gen_nntp/commit/484a8b65b1e570789b420f1c8d75c92b89c60017))
* handle_LISTGROUP/2 ([99fe433](https://github.com/sntran/gen_nntp/commit/99fe4337cb7b02aacf8ea65b2900235214a40ede))
* HEAD, BODY and STAT commands ([73f3be7](https://github.com/sntran/gen_nntp/commit/73f3be77ca883ea22f8a986712b8556872c9bb77))
* initial server behaviour ([84a396d](https://github.com/sntran/gen_nntp/commit/84a396d035b69f152f1e0f195cdfaeca4620fde2))
* make handle_command/2 optional ([c184f73](https://github.com/sntran/gen_nntp/commit/c184f73526a04229c906a33fee3a550e22c10794))
* NEXT command ([77f8d26](https://github.com/sntran/gen_nntp/commit/77f8d26831ee7a99a64f78ad07a50079b566adf2))
* port from env variable ([682acf8](https://github.com/sntran/gen_nntp/commit/682acf8ddf9a3586fc4a4632e0dd2d9aa9ecd0a8))
* send/3 to sends to a NNTP socket ([8ad00ec](https://github.com/sntran/gen_nntp/commit/8ad00ec428a01733a432f8e65534cff1159e85eb))
* NServ example ([1a98824](https://github.com/sntran/gen_nntp/commit/1a9882448b7d1e2de8ce7e04a7ebd8c4137fbd2a))

### Bug Fixes

* error codes for NEXT and LAST ([5414832](https://github.com/sntran/gen_nntp/commit/5414832c1ce54e2243ffcfd75e7ca1b2035cd919))
* HEAD, BODY and STAT callbacks ([35511a7](https://github.com/sntran/gen_nntp/commit/35511a7af9b7e9171f7a170185d4a6918a75a1e8))

* `send/3` becomes `command/3` ([94b0820](https://github.com/sntran/gen_nntp/commit/94b08204e39d5128dbae60ea995446dd29d5fc08))

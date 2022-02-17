;; ─┐ ┬┌─┐┌┐┌┌─┐┌┐ ┬ ┬┌┬┐┌─┐ ─┐ ┬┬ ┬┌─┐
;; ┌┴┬┘├┤ ││││ │├┴┐└┬┘ │ ├┤  ┌┴┬┘└┬┘┌─┘
;; ┴ └─└─┘┘└┘└─┘└─┘ ┴  ┴ └─┘o┴ └─ ┴ └─┘
;; Author:  SENEX @ XENOBYTE.XYZ
;; License: MIT License
;; Website: https://xenobyte.xyz/projects/?nav=hexmacs

;; Include all your packages in the list to have them automatically downloaded, validated
;; and updated
(load "~/.emacs.d/hexmacs-packages.el")

;; MODULE LOADING
;; Keep everything relevant in the same file. This greatly
;; aids in debugging and keeping track of what you actually
;; use.
;; Only enable helm or ivy
(require 'helm-conf)
;; (require 'ivy-conf)
(require 'flycheck-conf)
(require 'lsp-conf)
(require 'company-conf)
(require 'yasnippet-conf)
(require 'asm-conf)
(require 'web-conf)
(require 'lua-conf)
(require 'python-conf)
(require 'c-conf)
(require 'keys-conf)
(require 'interface-conf)
(require 'general-conf)

;; Might clash with emacs config face settings
(setq inhibit-x-resources 't)

;; User details
(setq user-full-name "SENEX")
(setq user-mail-address "xenobyte.xyz@protonmail.com")

;; Warning level
;; Careful, if you completely disable them you may end up making an otherwise simple debugging
;; process a complete nightmare
(setq warning-minimum-level :emergency)

;; Show a buffer with debug info on error
;; (setq debug-on-error t)

;; Backup files
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
;; Or disable them (at your own risk!)
;; (setq make-backup-files nil)

;; Automatically recompile everything that may need it
;; KEEPING THIS ENABLED SLOWS DOWN INITIALIZATION
(byte-recompile-directory (expand-file-name "~/.emacs.d") 0)

;;END CUSTOM EMACS CONFIGURATION
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#1a1a1a" "#d88568" "#83a471" "#b99353" "#8eccdd" "#b98eb2" "#8eccdd" "#cccccc"])
 '(beacon-color "#F8BBD0")
 '(company-quickhelp-color-background "#b0b0b0")
 '(company-quickhelp-color-foreground "#232333")
 '(custom-enabled-themes '(base16-tomorrow-night))
 '(custom-safe-themes
   '("7eb89cae66cc09050b2a2f64ead7675d9c992624263c5de2722f6112f8faa438" "c807d9bace60de158f7dfaf5fc0095bd740ef6dd43694da488bdb83b921cc09d" "c660dac3a164c6d324d8b982ffab9f955b7efacf73d2f7e64409c5d1681db7e9" "d1526f3110ea11342525f20e04c4847c3ac4eded9ebfd6ab8f064462a6505e1f" "863ff2daced12ebbbe16e17d0b6f63a9f5638903fa18d670d42f020b7df3bd6c" "db93ad9be9b2471fac5d0c2c108e2ba2908eff0e1d05a15a4507c7002998b5ea" "6b3507c8e8ffde783218f5f087889fd416095971a59205bb8443ee69aff51b6c" "c1449530bb797c8af035463c60dcb29479deefea012606cca69edd721de03c9a" "f711ee0edc8895b37f455593286c48562c96e62356b61990da5da4f0eec8475a" "e444ec67d5ee8f85bc458cb6d6570407b369d260a045275a3c6a9b88fefb2540" "c11a208be1ca4a73092ed04c159d8e99897466313389b2827adf5da9124ce7cf" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "bddc5cd70a98675ef72f4d59b01385540c39a8ab1c2b9921a1522dfa70ffa626" "7d9edb10f60c3292e2bbb18c274ae1fceb713f64fc9ab9c775ca5d4760c601b4" "1f538e3de6aa8711e3ad8f297a57ee8210ade0f46f411c06343d126a4fea72c6" "ecd8267ae99aa12d766f37bea093a6a3e3138494416956dcad29182e0fad69f8" "7220c44ef252ec651491125f1d95ad555fdfdc88f872d3552766862d63454582" "94660e5af9ca9d8b22139e5832869959048df22f554408aef239b275ba01d943" "39b4cfc179c8e97489f028bfce47a1fd81afdb4fc97bde9dbe5e1309253abfe5" "986e7e8e428decd5df9e8548a3f3b42afc8176ce6171e69658ae083f3c06211c" "96be1c5bb74fc2ffdfed87e46c87f1492969bf2af1fc96232e35c06b517aecc1" "6b423e7cd5d1e705b0c92356273899057d26e125f83d7f007615ae088195682f" "15e73942e742d19654b30515d908a226eb709b7a5622d0a057b5ae290e3e4219" "13ecdba8b8bdc126393c1a389b7b3e411e8ae3f8948ddabd160ebfa0501d728c" "e5c9860ef1654cd06ffb867265707e63d0dbf10ea1fce7654dd031ab634fd130" "dea5507be295b466872240242614832e37136a0941e7253062dfe3834953de0d" "50b64810ed1c36dfb72d74a61ae08e5869edc554102f20e078b21f84209c08d1" "eae831de756bb480240479794e85f1da0789c6f2f7746e5cc999370bbc8d9c8a" "7a1190ad27c73888f8d16142457f59026b01fa654f353c17f997d83565c0fc65" "4bf5c18667c48f2979ead0f0bdaaa12c2b52014a6abaa38558a207a65caeb8ad" "df21cdadd3f0648e3106338649d9fea510121807c907e2fd15565dde6409d6e9" "cbd8e65d2452dfaed789f79c92d230aa8bdf413601b261dbb1291fb88605110c" "c23a91feb89386e2fee8a8ac6c5766b3ce430e97f3f391ba7efc857574da0d51" "ffe80c88e3129b2cddadaaf78263a7f896d833a77c96349052ad5b7753c0c5a5" "64e3f7a494ad5491dea52e49253ee807be7799a4758822725fe29682abfe0cf7" "f66abed5139c808607639e5a5a3b5b50b9db91febeae06f11484a15a92bde442" "f6f5d5adce1f9a764855c9730e4c3ef3f90357313c1cae29e7c191ba1026bc15" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "c9f102cf31165896631747fd20a0ca0b9c64ecae019ce5c2786713a5b7d6315e" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "78c1c89192e172436dbf892bd90562bc89e2cc3811b5f9506226e735a953a9c6" "d9741f492c26b4e1c93874ee10476ca233e496827740b3fdb3aa6b6df871d449" "461e9e0d69636be8b5347a030f14b16c996c60a89e48c33f48bde51c32da3248" "376f42fd34de7bfdb4b6cc6f27bc271881eb5757f92e428260083412f036ef52" "85e6bb2425cbfeed2f2b367246ad11a62fb0f6d525c157038a0d0eaaabc1bfee" "d9850d120be9d94dd7ae69053630e89af8767c36b131a3aa7b06f14007a24656" "7a2ac0611ded83cdd60fc4de55ba57d36600eae261f55a551b380606345ee922" "80930c775cef2a97f2305bae6737a1c736079fdcc62a6fdf7b55de669fbbcd13" "350dc341799fbbb81e59d1e6fff2b2c8772d7000e352a5c070aa4317127eee94" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "d9dab332207600e49400d798ed05f38372ec32132b3f7d2ba697e59088021555" "250268d5c0b4877cc2b7c439687f8145a2c85a48981f7070a72c7f47a2d2dc13" "d96587ec2c7bf278269b8ec2b800c7d9af9e22d816827639b332b0e613314dfd" "eeefb10d84c6a058e39ae920a91b84afb1be2bdd3c6154ef43f1f0c922b19024" "5b8eccff13d79fc9b26c544ee20e1b0c499587d6c4bfc38cabe34beaf2c2fc77" "3f67aee8f8d8eedad7f547a346803be4cc47c420602e19d88bdcccc66dba033b" "ed36f8e30f02520ec09be9d74fe2a49f99ce85a3dfdb3a182ccd5f182909f3ab" "817e0f1f080b736d093e027415cd18957831f84a5c95b7963f95797bf5f84f2c" "50de25a66083652eca73a8040c2bff2dfa7e5ab44d16f684b740c7da658149fb" "35a7f5bf7b2958d5c156f9707f58c84d5a14e583f0e568a731f18f6b8a376f9d" "1bb8f76bcd04a2b25a663a3da69235fbdbe9db1d5fe7efc6e8fcfc5e1030c9c3" "1ac48c0de33d69da64e756db9c4c362429c09cfebbff5229431e6ee726aca3c0" "bedf9d7697f95381074fb2f02f417915b03a1c78ce207445a3a2178d44e36887" "a1eaf7074cc170883d2fd172e8d871cc29f2e97349bf42d9520c65b2d59c0411" "b6493078dc17528c57a3eb6b334e496217d026d856261b349603c9845f3cf2d8" "0ab2aa38f12640ecde12e01c4221d24f034807929c1f859cbca444f7b0a98b3a" "4863abd517bdc4a14388d6c87f960b0693cc1660db1d3495f4febcaec726cb1c" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "095d9d04dedae075f2593818e4f345f958e5d15cc494f8366d7e20ac685757ce" "d0cd794f302f503c2b605a305be05448d5a531412f548460e76ef7c29d64d8c7" "a056d45e5f6467d7407a3646c7b7768cf56642df17ed0223d401dea7d4d22f19" "ecfd522bd04e43c16e58bd8af7991bc9583b8e56286ea0959a428b3d7991bbd8" "d583e413c94b50882768e499db73059969617344935753c92c8a0b6323e701b9" "8150b36e4a45c579241de050c13178a8ee93693c6b9038145416746f2c030053" "23ba4b4ba4d1c989784475fed58919225db8d9a9751b32aa8df835134fe7ba6f" "043c8375cad0cf1d5c42f5d85cbed601075caf09594da04a74712510e9437d2b" "68fb87a1805393d7a00ba5064d28b8277de5a75addf0d34094012408cfcf2eea" "59ba50f24540958f33699a5247255d10f34dd812f3975837e3eddccdc4caa32e" "8543b328ed10bc7c16a8a35c523699befac0de00753824d7e90148bca583f986" "d8e3a2b8c72c3cb52d070a5e1969849197488b92d7211cc86c97e033239fdde2" "4b2679eac1095b60c2065187d713c39fbba27039d75c9c928a1f3b5d824a3b18" "bcfc77fcc3e012941eb47d5037f0fac767e23fd2dae039214e5fa856ac8bdfdd" "fec45178b55ad0258c5f68f61c9c8fd1a47d73b08fb7a51c15558d42c376083d" "6271fc9740379f8e2722f1510d481c1df1fcc43e48fa6641a5c19e954c21cc8f" "68c6d3938398ff6f2f28643ac839502668bb93186336eac8241b54c7c846dde4" "ad16a1bf1fd86bfbedae4b32c269b19f8d20d416bd52a87cd50e355bf13c2f23" "d83e34e28680f2ed99fe50fea79f441ca3fddd90167a72b796455e791c90dc49" "100eeb65d336e3d8f419c0f09170f9fd30f688849c5e60a801a1e6addd8216cb" "fee4e306d9070a55dce4d8e9d92d28bd9efe92625d2ba9d4d654fc9cd8113b7f" "50d07ab55e2b5322b2a8b13bc15ddf76d7f5985268833762c500a90e2a09e7aa" "4feee83c4fbbe8b827650d0f9af4ba7da903a5d117d849a3ccee88262805f40d" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "4a91a64af7ff1182ed04f7453bb5a4b0c3d82148d27db699df89a5f1d449e2a4" "8cf1002c7f805360115700144c0031b9cfa4d03edc6a0f38718cef7b7cabe382" "12670281275ea7c1b42d0a548a584e23b9c4e1d2dabb747fd5e2d692bcd0d39b" "36746ad57649893434c443567cb3831828df33232a7790d232df6f5908263692" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "afd6537452f2a7c37477c6ef775009010336bc0c1d267e1b7d072d7fc7e45249" "bbb521edff9940ba05aeeb49f9b247e95e1cb03bd78de18122f13500bda6514f" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "81e5f2b5415bfc35064e0e8cf9907a415378e9a56fe8845eed302a7b46a69351" "6ae2604614bee865c1e6bc98fdf39356a5a9ada610c24fe3e4491aecad681815" "c7a3537791082bfbfb44091bef32d20a18e992d5da0198bf8d8363221cca5b01" "fe15a6144a4f91dbd136c9d2ca74b1e13f502ac5c0b8a35d7daa2a97d2eabd29" "ed49a2f0e2f329308a17a0d9ecdbe7dc05d440554f0e7bfd1497778f8ffde877" "73ad471d5ae9355a7fa28675014ae45a0589c14492f52c32a4e9b393fcc333fd" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "888d95cf0e20998aef15a04969bedef99baa889053108d61e20f643e651d145a" "47f188b3ae4a3cdf29b54d3cf4b09ea1ec2cd15253879bfebbc568e86a25a81d" "b8929cff63ffc759e436b0f0575d15a8ad7658932f4b2c99415f3dde09b32e97" "a02d067c4f353583762ddb24406765b5110a08ce9f39571667d198eb37b78a46" "2a998a3b66a0a6068bcb8b53cd3b519d230dd1527b07232e54c8b9d84061d48d" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "08e0ba7881c93bc4ecb393df5de4c696ee820d586872ab5d42bb26834c9770eb" "a2af5ccd1febbaa9ab5ff26101606da70dfeda3b8f2cf33ac53632e81de37502" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "40555be85ace799a2952a93012fe8747bab0c2188c8a511f20f2b2b21c7cd62e" "c25d5ef5851d3036897e21f66d26bc1fa2a5ba89dbd15540604884a2fd163cc1" "bd0a135f51431fd129c47fbf14a812d2436fc3b1a37c8246f23950c12f25cce6" "f984e2f9765a69f7394527b44eaa28052ff3664a505f9ec9c60c088ca4e9fc0b" "9c4acf7b5801f25501f0db26ac3eee3dc263ed51afd01f9dcfda706a15234733" "3be1f5387122b935a26e02795196bc90860c57a62940f768f138b02383d9a257" "146061a7ceea4ccc75d975a3bb41432382f656c50b9989c7dc1a7bb6952f6eb4" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "264b639ee1d01cd81f6ab49a63b6354d902c7f7ed17ecf6e8c2bd5eb6d8ca09c" "e1498b2416922aa561076edc5c9b0ad7b34d8ff849f335c13364c8f4276904f0" "808b47c5c5583b5e439d8532da736b5e6b0552f6e89f8dafaab5631aace601dd" "1025e775a6d93981454680ddef169b6c51cc14cea8cb02d1872f9d3ce7a1da66" "44961a9303c92926740fc4121829c32abca38ba3a91897a4eab2aa3b7634bed4" "5a39d2a29906ab273f7900a2ae843e9aa29ed5d205873e1199af4c9ec921aaab" "840db7f67ce92c39deb38f38fbc5a990b8f89b0f47b77b96d98e4bf400ee590a" "7c0495f3973b9f79251205995ccccca41262b41a86553f81efe71c0dc3a50f43" "ef403aa0588ca64e05269a7a5df03a5259a00303ef6dfbd2519a9b81e4bce95c" "e6a9337674f6c967311b939bb4f81aefb65a96908c3749f4dd8d4500f6d79242" "91375c6dc506913ac7488f655b5afe934f343a0b223021c349105d37748c6696" "1d3863142a1325c1d038905c82b9aaf83f7594bb6158b52ad32ed23d3a97490a" "31e9b1ab4e6ccb742b3b5395287760a0adbfc8a7b86c2eda4555c8080a9338d9" "fb44ced1e15903449772b750c081e6b8f687732147aa43cfa2e7d9a38820744b" "46720e46428c490e7b2ddeafc2112c5a796c8cf4af71bd6b758d5c19316aff06" "8e51e44e5b079b2862335fcc5ff0f1e761dc595c7ccdb8398094fb8e088b2d50" "c2efd2e2e96b052dd91940b100d86885337a37be1245167642451cf6da5b924a" "65f35d1e0d0858947f854dc898bfd830e832189d5555e875705a939836b53054" "a62f0662e6aa7b05d0b4493a8e245ab31492765561b08192df61c9d1c7e1ddee" "819d24b9aba8fcb446aecfb59f87d1817a6d3eb07de7fdec67743ef32194438b" "04790c9929eacf32d508b84d34e80ad2ee233f13f17767190531b8b350b9ef22" "428bdd4b98d4d58cd094e7e074c4a82151ad4a77b9c9e30d75c56dc5a07f26c5" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "b0c5c6cc59d530d3f6fbcfa67801993669ce062dda1435014f74cafac7d86246" "542e6fee85eea8e47243a5647358c344111aa9c04510394720a3108803c8ddd1" "304c39b190267e9b863c0cf9c989da76dcfbb0649cbcb89592e7c5c08348fce9" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "ea6fd1ecc660fb434e19ed6b6d193bd25670bfcb6a955dfa21662da90184ba0c" "a0bfb4d94ef0a0893a9b19628403c5ac4847c981c8942a50fde0f273df47424a" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "69e7e7069edb56f9ed08c28ccf0db7af8f30134cab6415d5cf38ec5967348a3c" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "732ccca2e9170bcfd4ee5070159923f0c811e52b019106b1fc5eaa043dff4030" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "0961d780bd14561c505986166d167606239af3e2c3117265c9377e9b8204bf96" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "b67b2279fa90e4098aa126d8356931c7a76921001ddff0a8d4a0541080dee5f6" "87d46d0ad89557c616d04bef34afd191234992c4eb955ff3c60c6aa3afc2e5cc" "fede08d0f23fc0612a8354e0cf800c9ecae47ec8f32c5f29da841fe090dfc450" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "aded4ec996e438a5e002439d58f09610b330bbc18f580c83ebaba026bbef6c82" "60e09d2e58343186a59d9ed52a9b13d822a174b33f20bdc1d4abb86e6b17f45b" "02940c38e51991e8ee8ac69537341149d56e9c88d57f2c357eeb1744daad1953" "074f60822c8a332b1500092daae2fe048e43a11072c70b6f0d249931bdbe55dc" "99c86852decaeb0c6f51ce8bd46e4906a4f28ab4c5b201bdc3fdf85b24f88518" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "cea3ec09c821b7eaf235882e6555c3ffa2fd23de92459751e18f26ad035d2142" "24714e2cb4a9d6ec1335de295966906474fdb668429549416ed8636196cb1441" "a61109d38200252de49997a49d84045c726fa8d0f4dd637fce0b8affaa5c8620" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "760ce657e710a77bcf6df51d97e51aae2ee7db1fba21bbad07aab0fa0f42f834" "34ed3e2fa4a1cb2ce7400c7f1a6c8f12931d8021435bad841fdc1192bd1cc7da" "b3bcf1b12ef2a7606c7697d71b934ca0bdd495d52f901e73ce008c4c9825a3aa" "5b7c31eb904d50c470ce264318f41b3bbc85545e4359e6b7d48ee88a892b1915" "fe36e4da2ca97d9d706e569024caa996f8368044a8253dc645782e01cd68d884" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "33ea268218b70aa106ba51a85fe976bfae9cf6931b18ceaf57159c558bbcd1e6" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a" "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "bf10bd6d21928bf87bc3032b498c62cb9d48c54c06d217c8b00bef8090e539f7" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "3d4df186126c347e002c8366d32016948068d2e9198c496093a96775cc3b3eaa" "7bef2d39bac784626f1635bd83693fae091f04ccac6b362e0405abf16a32230c" "3de3f36a398d2c8a4796360bfce1fa515292e9f76b655bb9a377289a6a80a132" "a85e40c7d2df4a5e993742929dfd903899b66a667547f740872797198778d7b5" "8be07a2c1b3a7300860c7a65c0ad148be6d127671be04d3d2120f1ac541ac103" "d2bd16a8bcf295dce0b70e1d2b5c17bb34cb28224a86ee770d56e6c22a565013" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "dcdd1471fde79899ae47152d090e3551b889edf4b46f00df36d653adc2bf550d" "3a5f04a517096b08b08ef39db6d12bd55c04ed3d43b344cf8bd855bde6d3a1ae" "5a21604c4b1f2df98e67cda2347b8f42dc9ce471a48164fcb8d3d52c3a0d10be" "4f77827c989554f290a8f98a123ea020550864fa43776ca219d9cc76f7c42a94" "446cc97923e30dec43f10573ac085e384975d8a0c55159464ea6ef001f4a16ba" default))
 '(diary-entry-marker 'font-lock-variable-name-face)
 '(diff-hl-show-hunk-posframe-internal-border-color "#ffffffffffff")
 '(emms-mode-line-icon-color "#358d8d")
 '(emms-mode-line-icon-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };"))
 '(evil-emacs-state-cursor '("#D50000" hbar))
 '(evil-insert-state-cursor '("#D50000" bar))
 '(evil-normal-state-cursor '("#F57F17" box))
 '(evil-visual-state-cursor '("#66BB6A" box))
 '(fci-rule-color "#c7c7c7")
 '(gnus-logo-colors '("#0d7b72" "#adadad") t)
 '(gnus-mode-line-image-cache
   '(image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #358d8d\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };") t)
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(helm-gtags-auto-update t)
 '(helm-gtags-ignore-case t)
 '(helm-gtags-path-style 'relative)
 '(highlight-indent-guides-auto-enabled nil)
 '(highlight-parentheses-colors '("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11"))
 '(highlight-symbol-colors
   '("#F57F17" "#66BB6A" "#0097A7" "#42A5F5" "#7E57C2" "#D84315"))
 '(highlight-symbol-foreground-color "#546E7A")
 '(highlight-tail-colors '(("#F8BBD0" . 0) ("#FAFAFA" . 100)))
 '(mlscroll-in-color "#e67fe67fe67f")
 '(mlscroll-out-color "#FAFAFA")
 '(nrepl-message-colors
   '("#336c6c" "#205070" "#0f2050" "#806080" "#401440" "#6c1f1c" "#6b400c" "#23733c"))
 '(package-selected-packages
   '(dracula-theme atom-dark-theme arjen-grey-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes ahungry-theme afternoon-theme abyss-theme base16-theme ggtags rebox2 popwin latex-preview-pane auctex goto-chg yaml-mode dumb-jump org-journal powerline majapahit-theme browse-kill-ring nlinum move-text mode-icons symon expand-region restclient drag-stuff zencoding-mode rainbow-delimiters rainbow-mode highlight-symbol highlight-numbers diff-hl vlf neotree ztree recentf-ext clean-aindent-mode duplicate-thing volatile-highlights undo-tree smartparens ws-butler counsel-etags x86-lookup nasm-mode iasm-mode py-autopep8 elpy indium tern xref-js2 js2-refactor js2-mode lsp-ui web-mode php-extras ac-php function-args yasnippet-snippets yasnippet flycheck-tip flycheck-irony flycheck company-ctags company-lsp company-glsl company-arduino company-jedi company-anaconda company-lua company-irony-c-headers company-irony company-web company-php auto-complete company counsel ivy-hydra swiper-helm helm-tramp helm-themes helm-gtags helm))
 '(pdf-view-midnight-colors '("#232333" . "#c7c7c7"))
 '(pos-tip-background-color "#303030")
 '(pos-tip-foreground-color "#78909C")
 '(red "#ffffff")
 '(tabbar-background-color "#ffffffffffff")
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map
   '((20 . "#437c7c")
     (40 . "#336c6c")
     (60 . "#205070")
     (80 . "#2f4070")
     (100 . "#1f3060")
     (120 . "#0f2050")
     (140 . "#a080a0")
     (160 . "#806080")
     (180 . "#704d70")
     (200 . "#603a60")
     (220 . "#502750")
     (240 . "#401440")
     (260 . "#6c1f1c")
     (280 . "#935f5c")
     (300 . "#834744")
     (320 . "#732f2c")
     (340 . "#6b400c")
     (360 . "#23733c")))
 '(vc-annotate-very-old-color "#23733c")
 '(xterm-color-names
   ["#303030" "#D66F84" "#D79887" "#D49A8A" "#94B1A3" "#A8938C" "#989584" "#BAB2A9"])
 '(xterm-color-names-bright
   ["#3A3A3A" "#E47386" "#CC816B" "#769188" "#7D6F6A" "#9C8772" "#BAB2A9"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

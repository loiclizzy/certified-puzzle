const MONTHS=["JAN","FEB","MAR","APR","MAY","JUN","JUL","AUG","SEP","OCT","NOV","DEC"];
const DAYS=[31,29,31,30,31,30,31,31,30,31,30,31];
const COLORS={A:"#72e35a",B:"#8be74f",C:"#a6eb5f",D:"#4fe0ba",E:"#62d9f3",F:"#8bb7ff",G:"#b1a6ff",H:"#db9cff"};
const I18N={"fr":{workflowEyebrow: "CERTIFICATION",
workflowTitle: "Fonctionnement de la certification",
workflowText: "Les solutions candidates sont générées d’abord, puis acceptées uniquement si le vérificateur Rocq confirme les règles.",
workflowAlt: "Flux de certification de Certified Puzzle","pageTitle":"Certified Puzzle","heroEyebrow":"CERTIFIED CALENDAR PUZZLE","heroTitle":"Certified Puzzle","authorByline":"Par CertiSen","heroSubtitle":"Choisissez un mois et un jour, générez une solution, faites-la vérifier par le vérificateur OCaml, puis suivez le guide pas à pas.","languageLabel":"Langue","engineLoading":"Chargement du vérificateur…","monthLabel":"Mois","dayLabel":"Jour","solveButton":"Générer et vérifier","chooseDate":"Choisissez une date","verificationPending":"La solution sera vérifiée après sa génération.","certifiedSubtitle":"Accepté par le vérificateur OCaml","guideEyebrow":"GUIDE MODE","guideTitle":"Guide pas à pas","guideEmpty":"Chargement du guide pour la date sélectionnée…","previousButton":"Précédent","nextButton":"Suivant","finishButton":"Terminer","showFuture":"Afficher en transparence les pièces à venir","certificateTitle":"Certificat vérifiable","notGenerated":"Pas encore généré.","copyCertificate":"Copier le certificat","copied":"Copié","engineReady":"Vérificateur OCaml prêt","enginePreview":"Aperçu développement · miroir JavaScript","engineError":"Échec du chargement","loadingVerification":"Vérification en cours…","noCertificate":"Aucun certificat trouvé pour cette date.","verified":"Vérification OCaml réussie : 41 cases couvertes, aucun chevauchement, date visible.","previewVerified":"Vérification d’aperçu réussie ; compilez avec js_of_ocaml pour afficher Certified.","verificationFailed":"Échec de la vérification : certificat refusé.","exposed":"visible","module":"Pièce","prepareBoard":"Placez correctement le plateau et laissez <strong>{month}</strong> et <strong>{day}</strong> visibles. Cliquez sur « Suivant » pour commencer.","stepInstruction":"Étape <strong>{step}</strong> : placez la pièce <strong>{piece}</strong>.<br>Elle couvre : <strong>{labels}</strong><br>Paramètres du certificat : orientation {orientation}, ancrage ({row}, {col}).","boardAria":"Plateau du puzzle calendrier","monthNames":["janvier","février","mars","avril","mai","juin","juillet","août","septembre","octobre","novembre","décembre"],"footerProject":"Certified Puzzle","footerAuthor":" · Auteur : CertiSen","footerTech":"Preuve Rocq · extraction OCaml · vérificateur web js_of_ocaml","footerGithub":"Code sur GitHub","manualEyebrow":"MANUAL BUILDER","manualTitle":"Construction manuelle","manualIntro":"Faites glisser les pièces sur le plateau, faites-les pivoter, puis demandez un conseil basé sur les pièces déjà placées.","pieceTrayTitle":"Pièces disponibles","manualBoardTitle":"Plateau manuel","selectedPiece":"Pièce sélectionnée","noneSelected":"Aucune","rotatePiece":"Faire pivoter","removePiece":"Retirer","resetManual":"Réinitialiser","checkManual":"Vérifier le placement","suggestNext":"Suggérer la prochaine étape","applySuggestion":"Placer la suggestion","manualReady":"Faites glisser une pièce sur une case du plateau. Sur mobile, touchez une pièce puis une case.","manualPartialValid":"Le placement partiel est légal. Vous pouvez continuer.","manualInvalid":"Le placement contient un chevauchement, une sortie du plateau ou couvre la date cible.","manualComplete":"Les huit pièces sont placées et le certificat est valide.","manualCompletePreview":"Les huit pièces sont placées et le vérificateur de prévisualisation les accepte.","manualNotComplete":"Placez les huit pièces avant la vérification complète.","pieceAlreadyPlaced":"Cette pièce est déjà placée. Sélectionnez-la sur le plateau pour la déplacer.","dropInvalid":"Cette position est illégale. La pièce a été remise à sa position précédente.","piecePlaced":"Pièce {piece} placée.","pieceRemoved":"Pièce {piece} retirée.","manualResetDone":"Le plateau manuel a été réinitialisé.","suggestFix":"La pièce {piece} ne correspond pas à la solution certifiée. Conseil : orientation {orientation}, ancrage ({row}, {col}), couvrant {labels}.","suggestPlace":"Prochaine pièce conseillée : {piece}, orientation {orientation}, ancrage ({row}, {col}), couvrant {labels}.","suggestDone":"Toutes les pièces correspondent à la solution certifiée. Lancez la vérification finale.","suggestInvalidFirst":"Corrigez d’abord les placements en conflit indiqués en rouge.","suggestNoSolution":"Aucune suggestion disponible pour cette date.","guidePlaceManual":"Placer cette pièce sur le plateau manuel","orientationLabel":"Orientation","anchorLabel":"Ancrage","placedLabel":"placée","unplacedLabel":"disponible","manualCertified":"CERTIFIED MANUAL BUILD","manualCertifiedSub":"Construction manuelle acceptée par le vérificateur OCaml"},
"en":{workflowEyebrow: "CERTIFICATION",
workflowTitle: "How certification works",
workflowText: "Candidate solutions are generated first, then accepted only if the Rocq verifier confirms the rules.",
workflowAlt: "Certified Puzzle certification workflow","pageTitle":"Certified Puzzle","heroEyebrow":"CERTIFIED CALENDAR PUZZLE","heroTitle":"Certified Puzzle","authorByline":"By CertiSen","heroSubtitle":"Choose a month and day, generate a solution, verify it with the OCaml verifier, and follow the step-by-step guide.","languageLabel":"Language","engineLoading":"Loading verifier…","monthLabel":"Month","dayLabel":"Day","solveButton":"Generate and verify","chooseDate":"Choose a date","verificationPending":"The solution will be verified after generation.","certifiedSubtitle":"Accepted by the OCaml verifier","guideEyebrow":"GUIDE MODE","guideTitle":"Step-by-step guide","guideEmpty":"Loading the guide for the selected date…","previousButton":"Previous","nextButton":"Next","finishButton":"Finish","showFuture":"Show future pieces faded","certificateTitle":"Machine-verifiable certificate","notGenerated":"Not generated yet.","copyCertificate":"Copy certificate","copied":"Copied","engineReady":"OCaml verifier ready","enginePreview":"Development preview · JavaScript mirror","engineError":"Loading failed","loadingVerification":"Running verifier…","noCertificate":"No certificate was found for this date.","verified":"OCaml verification passed: 41 cells covered, no overlap, target date remains visible.","previewVerified":"Preview verification passed; compile with js_of_ocaml to display Certified.","verificationFailed":"Verification failed: certificate rejected.","exposed":"visible","module":"Piece","prepareBoard":"Orient the board correctly and leave <strong>{month}</strong> and <strong>{day}</strong> visible. Click “Next” to begin.","stepInstruction":"Step <strong>{step}</strong>: place piece <strong>{piece}</strong>.<br>It covers: <strong>{labels}</strong><br>Certificate parameters: orientation {orientation}, anchor ({row}, {col}).","boardAria":"Calendar puzzle board","monthNames":["January","February","March","April","May","June","July","August","September","October","November","December"],"footerProject":"Certified Puzzle","footerAuthor":" · Author: CertiSen","footerTech":"Rocq proof · OCaml extraction · js_of_ocaml web verifier","footerGithub":"Code on GitHub","manualEyebrow":"MANUAL BUILDER","manualTitle":"Manual construction","manualIntro":"Drag pieces onto the board, rotate them, then ask for a recommendation based on the pieces already placed.","pieceTrayTitle":"Available pieces","manualBoardTitle":"Manual board","selectedPiece":"Selected piece","noneSelected":"None","rotatePiece":"Rotate","removePiece":"Remove","resetManual":"Reset","checkManual":"Verify placement","suggestNext":"Suggest next step","applySuggestion":"Place suggestion","manualReady":"Drag a piece onto a board cell. On mobile, tap a piece and then tap a cell.","manualPartialValid":"The partial placement is legal. You can continue.","manualInvalid":"The placement has an overlap, leaves the board, or covers the target date.","manualComplete":"All eight pieces are placed and the certificate is valid.","manualCompletePreview":"All eight pieces are placed and accepted by the preview verifier.","manualNotComplete":"Place all eight pieces before full verification.","pieceAlreadyPlaced":"This piece is already placed. Select it on the board to move it.","dropInvalid":"That position is illegal. The piece was restored to its previous position.","piecePlaced":"Piece {piece} placed.","pieceRemoved":"Piece {piece} removed.","manualResetDone":"The manual board has been reset.","suggestFix":"Piece {piece} does not match the certified solution. Recommendation: orientation {orientation}, anchor ({row}, {col}), covering {labels}.","suggestPlace":"Recommended next piece: {piece}, orientation {orientation}, anchor ({row}, {col}), covering {labels}.","suggestDone":"Every piece matches the certified solution. Run the final verification.","suggestInvalidFirst":"First fix the conflicting placements highlighted in red.","suggestNoSolution":"No recommendation is available for this date.","guidePlaceManual":"Place this piece on the manual board","orientationLabel":"Orientation","anchorLabel":"Anchor","placedLabel":"placed","unplacedLabel":"available","manualCertified":"CERTIFIED MANUAL BUILD","manualCertifiedSub":"Manual construction accepted by the OCaml verifier"},
"zh":{workflowEyebrow: "CERTIFICATION",
workflowTitle: "认证流程如何工作",
workflowText: "候选解先由生成器产生，只有被 Rocq 验证器确认满足规则后，才会被接受为 certified solution。",
workflowAlt: "Certified Puzzle 认证流程图","pageTitle":"Certified Puzzle","heroEyebrow":"CERTIFIED CALENDAR PUZZLE","heroTitle":"Certified Puzzle","authorByline":"作者：CertiSen","heroSubtitle":"选择月份和日期，生成拼法；通过 OCaml 验证器后显示 Certified 标志，并可逐步跟随拼图指南。","languageLabel":"语言","engineLoading":"正在加载验证器…","monthLabel":"月份","dayLabel":"日期","solveButton":"生成并验证","chooseDate":"请选择日期","verificationPending":"生成拼法后将进行验证。","certifiedSubtitle":"OCaml 验证器已接受","guideEyebrow":"GUIDE MODE","guideTitle":"逐步拼图","guideEmpty":"正在载入所选日期的逐步指南…","previousButton":"上一步","nextButton":"下一步","finishButton":"完成","showFuture":"淡化显示尚未放置的模块","certificateTitle":"机器可验证证书","notGenerated":"尚未生成。","copyCertificate":"复制证书","copied":"已复制","engineReady":"OCaml 验证器已就绪","enginePreview":"开发预览 · JavaScript 镜像","engineError":"加载失败","loadingVerification":"正在运行验证器…","noCertificate":"证书表中没有找到该日期。","verified":"OCaml 验证通过：41 格覆盖、无重叠、目标日期保持露出。","previewVerified":"开发预览验证通过；编译 js_of_ocaml 后才会显示 Certified。","verificationFailed":"验证失败，证书被拒绝。","exposed":"露出","module":"模块","prepareBoard":"先把棋盘摆正，确认 <strong>{month}</strong> 和 <strong>{day}</strong> 两格保持露出。点击“下一步”开始。","stepInstruction":"第 <strong>{step}</strong> 步：放置模块 <strong>{piece}</strong>。<br>覆盖：<strong>{labels}</strong><br>证书参数：方向 {orientation}，锚点 ({row}, {col})。","boardAria":"日期拼图棋盘","monthNames":["一月","二月","三月","四月","五月","六月","七月","八月","九月","十月","十一月","十二月"],"footerProject":"Certified Puzzle","footerAuthor":" · 作者：CertiSen","footerTech":"Rocq 证明 · OCaml 抽取 · js_of_ocaml 网页验证器","footerGithub":"GitHub 代码","manualEyebrow":"MANUAL BUILDER","manualTitle":"手动构建","manualIntro":"把各个模块拖到棋盘上，可以旋转和移动；系统会根据当前已放置的模块给出下一步建议。","pieceTrayTitle":"可用模块","manualBoardTitle":"手动棋盘","selectedPiece":"已选择模块","noneSelected":"无","rotatePiece":"旋转","removePiece":"移除","resetManual":"重置","checkManual":"验证当前拼法","suggestNext":"建议下一步","applySuggestion":"放置建议模块","manualReady":"将模块拖到棋盘格。手机上可以先点击模块，再点击棋盘格。","manualPartialValid":"当前部分拼法合法，可以继续。","manualInvalid":"当前拼法存在重叠、越界，或覆盖了目标日期。","manualComplete":"八块模块均已放置，证书验证通过。","manualCompletePreview":"八块模块均已放置，并通过预览验证器。","manualNotComplete":"完整验证前需要放置全部八块模块。","pieceAlreadyPlaced":"该模块已经放置。请在棋盘上选中后移动。","dropInvalid":"该位置不合法，模块已恢复到之前的位置。","piecePlaced":"模块 {piece} 已放置。","pieceRemoved":"模块 {piece} 已移除。","manualResetDone":"手动棋盘已重置。","suggestFix":"模块 {piece} 与认证解不一致。建议：方向 {orientation}，锚点 ({row}, {col})，覆盖 {labels}。","suggestPlace":"建议下一步放置模块 {piece}：方向 {orientation}，锚点 ({row}, {col})，覆盖 {labels}。","suggestDone":"所有模块都与认证解一致，请执行最终验证。","suggestInvalidFirst":"请先修正红色标出的冲突模块。","suggestNoSolution":"当前日期没有可用建议。","guidePlaceManual":"把当前模块放到手动棋盘","orientationLabel":"方向","anchorLabel":"锚点","placedLabel":"已放置","unplacedLabel":"可使用","manualCertified":"CERTIFIED MANUAL BUILD","manualCertifiedSub":"手动构建已通过 OCaml 验证器"},
"ar":{workflowEyebrow: "CERTIFICATION",
workflowTitle: "كيف تعمل عملية الاعتماد",
workflowText: "تُولَّد الحلول المرشحة أولاً، ولا تُقبَل إلا إذا أكد مدقق Rocq أنها تلتزم بالقواعد.",
workflowAlt: "مخطط سير اعتماد Certified Puzzle","pageTitle":"Certified Puzzle","heroEyebrow":"CERTIFIED CALENDAR PUZZLE","heroTitle":"Certified Puzzle","authorByline":"إعداد CertiSen","heroSubtitle":"اختر الشهر واليوم، ثم أنشئ حلاً وتحقق منه بواسطة مدقق OCaml، أو اتبع الدليل خطوة بخطوة.","languageLabel":"اللغة","engineLoading":"جارٍ تحميل المدقق…","monthLabel":"الشهر","dayLabel":"اليوم","solveButton":"إنشاء الحل والتحقق","chooseDate":"اختر تاريخًا","verificationPending":"سيتم التحقق من الحل بعد إنشائه.","certifiedSubtitle":"تم قبوله بواسطة مدقق OCaml","guideEyebrow":"GUIDE MODE","guideTitle":"دليل خطوة بخطوة","guideEmpty":"جارٍ تحميل الدليل للتاريخ المحدد…","previousButton":"السابق","nextButton":"التالي","finishButton":"إنهاء","showFuture":"إظهار القطع التالية بشكل باهت","certificateTitle":"شهادة قابلة للتحقق آليًا","notGenerated":"لم يتم الإنشاء بعد.","copyCertificate":"نسخ الشهادة","copied":"تم النسخ","engineReady":"مدقق OCaml جاهز","enginePreview":"معاينة التطوير · مدقق JavaScript","engineError":"فشل التحميل","loadingVerification":"جارٍ التحقق…","noCertificate":"لم يتم العثور على شهادة لهذا التاريخ.","verified":"نجح تحقق OCaml: تمت تغطية 41 خانة دون تداخل، وبقي التاريخ ظاهرًا.","previewVerified":"نجح تحقق المعاينة؛ قم بالترجمة باستخدام js_of_ocaml لإظهار علامة Certified.","verificationFailed":"فشل التحقق: تم رفض الشهادة.","exposed":"ظاهر","module":"القطعة","prepareBoard":"ضع اللوحة بالاتجاه الصحيح واترك <strong>{month}</strong> و<strong>{day}</strong> ظاهرين. اضغط «التالي» للبدء.","stepInstruction":"الخطوة <strong>{step}</strong>: ضع القطعة <strong>{piece}</strong>.<br>تغطي: <strong>{labels}</strong><br>معاملات الشهادة: الاتجاه {orientation}، نقطة الارتكاز ({row}, {col}).","boardAria":"لوحة لغز التقويم","monthNames":["يناير","فبراير","مارس","أبريل","مايو","يونيو","يوليو","أغسطس","سبتمبر","أكتوبر","نوفمبر","ديسمبر"],"footerProject":"Certified Puzzle","footerAuthor":" · المؤلف: CertiSen","footerTech":"إثبات Rocq · استخراج OCaml · مدقق ويب js_of_ocaml","footerGithub":"الكود على GitHub","manualEyebrow":"MANUAL BUILDER","manualTitle":"البناء اليدوي","manualIntro":"اسحب القطع إلى اللوحة، وقم بتدويرها أو تحريكها، ثم اطلب اقتراحًا اعتمادًا على القطع الموضوعة حاليًا.","pieceTrayTitle":"القطع المتاحة","manualBoardTitle":"اللوحة اليدوية","selectedPiece":"القطعة المحددة","noneSelected":"لا توجد","rotatePiece":"تدوير","removePiece":"إزالة","resetManual":"إعادة ضبط","checkManual":"تحقق من الوضع الحالي","suggestNext":"اقتراح الخطوة التالية","applySuggestion":"وضع القطعة المقترحة","manualReady":"اسحب قطعة إلى إحدى خانات اللوحة. على الهاتف، اضغط القطعة ثم اضغط الخانة.","manualPartialValid":"الوضع الجزئي صحيح ويمكنك المتابعة.","manualInvalid":"يوجد تداخل أو خروج خارج اللوحة أو تغطية للتاريخ المطلوب.","manualComplete":"تم وضع القطع الثماني والشهادة صحيحة.","manualCompletePreview":"تم وضع القطع الثماني وقبلها مدقق المعاينة.","manualNotComplete":"ضع القطع الثماني قبل إجراء التحقق الكامل.","pieceAlreadyPlaced":"هذه القطعة موضوعة بالفعل. حددها على اللوحة لتحريكها.","dropInvalid":"هذا الموضع غير صالح. تمت إعادة القطعة إلى موضعها السابق.","piecePlaced":"تم وضع القطعة {piece}.","pieceRemoved":"تمت إزالة القطعة {piece}.","manualResetDone":"تمت إعادة ضبط اللوحة اليدوية.","suggestFix":"القطعة {piece} لا تطابق الحل المعتمد. الاقتراح: الاتجاه {orientation}، نقطة الارتكاز ({row}, {col})، وتغطي {labels}.","suggestPlace":"القطعة التالية المقترحة: {piece}، الاتجاه {orientation}، نقطة الارتكاز ({row}, {col})، وتغطي {labels}.","suggestDone":"كل القطع تطابق الحل المعتمد. نفّذ التحقق النهائي.","suggestInvalidFirst":"صحح أولًا المواضع المتعارضة المميزة باللون الأحمر.","suggestNoSolution":"لا يوجد اقتراح متاح لهذا التاريخ.","guidePlaceManual":"ضع هذه القطعة على اللوحة اليدوية","orientationLabel":"الاتجاه","anchorLabel":"نقطة الارتكاز","placedLabel":"موضوعة","unplacedLabel":"متاحة","manualCertified":"CERTIFIED MANUAL BUILD","manualCertifiedSub":"تم قبول البناء اليدوي بواسطة مدقق OCaml"}};
const variants={"A":[[[0,0],[0,1],[0,2],[1,2],[1,3]],[[0,0],[0,1],[1,1],[1,2],[1,3]],[[0,0],[1,0],[1,1],[2,1],[3,1]],[[0,0],[1,0],[2,0],[2,1],[3,1]],[[0,1],[0,2],[0,3],[1,0],[1,1]],[[0,1],[1,0],[1,1],[2,0],[3,0]],[[0,1],[1,1],[2,0],[2,1],[3,0]],[[0,2],[0,3],[1,0],[1,1],[1,2]]],"B":[[[0,0],[0,1],[0,2],[1,0],[1,1],[1,2]],[[0,0],[0,1],[1,0],[1,1],[2,0],[2,1]]],"C":[[[0,0],[0,1],[0,2],[0,3],[1,0]],[[0,0],[0,1],[0,2],[0,3],[1,3]],[[0,0],[0,1],[1,0],[2,0],[3,0]],[[0,0],[0,1],[1,1],[2,1],[3,1]],[[0,0],[1,0],[1,1],[1,2],[1,3]],[[0,0],[1,0],[2,0],[3,0],[3,1]],[[0,1],[1,1],[2,1],[3,0],[3,1]],[[0,3],[1,0],[1,1],[1,2],[1,3]]],"D":[[[0,0],[0,1],[1,1],[2,1],[2,2]],[[0,0],[1,0],[1,1],[1,2],[2,2]],[[0,1],[0,2],[1,1],[2,0],[2,1]],[[0,2],[1,0],[1,1],[1,2],[2,0]]],"E":[[[0,0],[0,1],[0,2],[1,0],[2,0]],[[0,0],[0,1],[0,2],[1,2],[2,2]],[[0,0],[1,0],[2,0],[2,1],[2,2]],[[0,2],[1,2],[2,0],[2,1],[2,2]]],"F":[[[0,0],[0,1],[0,2],[0,3],[1,1]],[[0,0],[0,1],[0,2],[0,3],[1,2]],[[0,0],[1,0],[1,1],[2,0],[3,0]],[[0,0],[1,0],[2,0],[2,1],[3,0]],[[0,1],[1,0],[1,1],[1,2],[1,3]],[[0,1],[1,0],[1,1],[2,1],[3,1]],[[0,1],[1,1],[2,0],[2,1],[3,1]],[[0,2],[1,0],[1,1],[1,2],[1,3]]],"G":[[[0,0],[0,1],[0,2],[1,0],[1,2]],[[0,0],[0,1],[1,0],[2,0],[2,1]],[[0,0],[0,1],[1,1],[2,0],[2,1]],[[0,0],[0,2],[1,0],[1,1],[1,2]]],"H":[[[0,0],[0,1],[0,2],[1,0],[1,1]],[[0,0],[0,1],[0,2],[1,1],[1,2]],[[0,0],[0,1],[1,0],[1,1],[1,2]],[[0,0],[0,1],[1,0],[1,1],[2,0]],[[0,0],[0,1],[1,0],[1,1],[2,1]],[[0,0],[1,0],[1,1],[2,0],[2,1]],[[0,1],[0,2],[1,0],[1,1],[1,2]],[[0,1],[1,0],[1,1],[2,0],[2,1]]]};
const PIECES="ABCDEFGH".split("");
const LABELS=new Map();for(let c=0;c<6;c++)LABELS.set(`0,${c}`,MONTHS[c]);for(let c=0;c<6;c++)LABELS.set(`1,${c}`,MONTHS[c+6]);let n=1;for(let r=2;r<6;r++)for(let c=0;c<7;c++)LABELS.set(`${r},${c}`,String(n++).padStart(2,"0"));for(let c=0;c<3;c++)LABELS.set(`6,${c}`,String(n++).padStart(2,"0"));
let solutionTable={},current=null,solutionRevealed=false,guideStep=0,currentLanguage=localStorage.getItem("calendarPuzzleLanguage")||"fr";
let manualPlacements={},selectedManualPiece=null,draggingPiece=null,currentSuggestion=null;
const $=id=>document.getElementById(id),monthSelect=$("month"),daySelect=$("day");
const t=k=>I18N[currentLanguage][k]??k;const fmt=(s,v)=>s.replace(/\{(\w+)\}/g,(_,k)=>v[k]??"");
function key(r,c){return `${r},${c}`}function monthCell(m){return m<=6?[0,m-1]:[1,m-7]}function dayCell(d){return d<=28?[2+Math.floor((d-1)/7),(d-1)%7]:[6,d-29]}
function placementCells(p){const s=variants[p.piece]?.[p.orientation];return s?s.map(([dr,dc])=>[p.row+dr,p.col+dc]):[]}
function samePlacement(a,b){return a&&b&&a.piece===b.piece&&a.orientation===b.orientation&&a.row===b.row&&a.col===b.col}
function certificateText(cert){return["# PIECE ORIENTATION ROW COLUMN",...cert.map(p=>`${p.piece} ${p.orientation} ${p.row} ${p.col}`)].join("\n")}
function targetSet(){if(!current)return new Set();const[mr,mc]=monthCell(current.month),[dr,dc]=dayCell(current.day);return new Set([key(mr,mc),key(dr,dc)])}
function applyTranslations(){document.documentElement.lang=currentLanguage==="zh"?"zh-CN":currentLanguage;document.documentElement.dir=currentLanguage==="ar"?"rtl":"ltr";document.title=t("pageTitle");document.querySelectorAll("[data-i18n]").forEach(x=>{const v=t(x.dataset.i18n);if(typeof v==="string")x.textContent=v});$("language").value=currentLanguage;$("board").setAttribute("aria-label",t("boardAria"));$("manualBoard").setAttribute("aria-label",t("manualBoardTitle"))}
function fillMonths(){const selected=Number(monthSelect.value)||6;monthSelect.innerHTML="";t("monthNames").forEach((name,i)=>{const o=document.createElement("option");o.value=i+1;o.textContent=`${name} · ${MONTHS[i]}`;monthSelect.appendChild(o)});monthSelect.value=String(selected)}
function fillDays(){const m=Number(monthSelect.value),previous=Number(daySelect.value)||9;daySelect.innerHTML="";for(let d=1;d<=DAYS[m-1];d++){const o=document.createElement("option");o.value=d;o.textContent=String(d).padStart(2,"0");daySelect.appendChild(o)}daySelect.value=String(Math.min(previous,DAYS[m-1]))}
function createShapeElement(piece,orientation=0,size="normal"){const shape=variants[piece][orientation],maxR=Math.max(...shape.map(x=>x[0])),maxC=Math.max(...shape.map(x=>x[1]));const el=document.createElement("div");el.className=`piece-shape ${size}`;el.style.gridTemplateRows=`repeat(${maxR+1},1fr)`;el.style.gridTemplateColumns=`repeat(${maxC+1},1fr)`;shape.forEach(([r,c])=>{const q=document.createElement("span");q.style.gridRow=r+1;q.style.gridColumn=c+1;q.style.background=COLORS[piece];el.appendChild(q)});return el}
function renderSolutionBoard(){
  const b=$("board");
  b.innerHTML="";
  const coverage=new Map(),targets=targetSet();
  if(current){
    current.cert.forEach((p,i)=>{
      const visible=solutionRevealed || i<guideStep;
      if(visible) placementCells(p).forEach(([r,c])=>coverage.set(key(r,c),{piece:p.piece,index:i}));
    });
  }
  for(let r=0;r<7;r++){
    for(let c=0;c<7;c++){
      const k=key(r,c),cell=document.createElement("div");
      cell.className="cell";
      cell.style.gridRow=r+1;
      cell.style.gridColumn=c+1;
      if(!LABELS.has(k)) cell.classList.add("empty");
      else{
        cell.textContent=LABELS.get(k);
        if(targets.has(k)){
          cell.classList.add("target");
          const sm=document.createElement("small");sm.textContent=t("exposed");cell.appendChild(sm);
        }else if(coverage.has(k)){
          const info=coverage.get(k);
          cell.style.background=COLORS[info.piece];
          const sm=document.createElement("small");sm.textContent=info.piece;cell.appendChild(sm);
          if(!solutionRevealed && info.index===guideStep-1) cell.classList.add("active");
        }else cell.style.background="#eef4ff";
      }
      b.appendChild(cell);
    }
  }
}
function renderLegend(){const l=$("legend");l.innerHTML="";PIECES.forEach(p=>{const x=document.createElement("span");x.className="legend-item";x.innerHTML=`<span class="legend-dot" style="background:${COLORS[p]}"></span>${t("module")} ${p}`;l.appendChild(x)})}
function coveredLabels(p){return placementCells(p).map(([r,c])=>LABELS.get(key(r,c))).filter(Boolean).join(currentLanguage==="zh"?"、":", ")}
function updateSelectedDate(){if(!current)return;const d=String(current.day).padStart(2,"0");$("selectedDate").textContent=`${t("monthNames")[current.month-1]} ${d} · ${MONTHS[current.month-1]} ${d}`}
function updateGuide(){if(!current){$("stepCounter").textContent="0 / 8";$("prevBtn").disabled=true;$("nextBtn").disabled=false;$("nextBtn").textContent=t("nextButton");$("guideInstruction").innerHTML=t("guideEmpty");return;}$("stepCounter").textContent=`${guideStep} / 8`;$("prevBtn").disabled=guideStep===0;$("nextBtn").disabled=guideStep===8;$("nextBtn").textContent=guideStep===7?t("finishButton"):t("nextButton");$("guidePiecePreview").innerHTML="";if(guideStep===0){$("guideInstruction").innerHTML=fmt(t("prepareBoard"),{month:MONTHS[current.month-1],day:String(current.day).padStart(2,"0")});$("guidePlaceBtn").disabled=true}else{const p=current.cert[guideStep-1];$("guidePiecePreview").appendChild(createShapeElement(p.piece,p.orientation,"large"));$("guideInstruction").innerHTML=fmt(t("stepInstruction"),{step:guideStep,piece:p.piece,labels:coveredLabels(p),orientation:p.orientation,row:p.row,col:p.col});$("guidePlaceBtn").disabled=false}renderSolutionBoard()}
function updateEngineStatus(){const v=window.CalendarOCamlVerifier,s=$("engineStatus");s.classList.remove("ok","dev");if(v?.engine==="rocq-extracted-ocaml"||v?.engine==="ocaml-js-of-ocaml"){s.textContent=t("engineReady");s.classList.add("ok")}else{s.textContent=t("enginePreview");s.classList.add("dev")}}
function updateVerificationMessage(){if(!current){$("verifyMessage").textContent=t("verificationPending");return}const v=window.CalendarOCamlVerifier;if(current.valid&&(v?.engine==="rocq-extracted-ocaml"||v?.engine==="ocaml-js-of-ocaml"))$("verifyMessage").textContent=t("verified");else if(current.valid)$("verifyMessage").textContent=t("previewVerified");else $("verifyMessage").textContent=t("verificationFailed")}
function renderPieceTray(){const tray=$("pieceTray");tray.innerHTML="";PIECES.forEach(piece=>{const placed=Boolean(manualPlacements[piece]);const card=document.createElement("button");card.type="button";card.className=`piece-card${selectedManualPiece===piece?" selected":""}${placed?" placed":""}`;card.draggable=!placed;card.dataset.piece=piece;card.appendChild(createShapeElement(piece,manualPlacements[piece]?.orientation||0,"mini"));const cap=document.createElement("span");cap.className="piece-caption";cap.textContent=`${piece} · ${placed?t("placedLabel"):t("unplacedLabel")}`;card.appendChild(cap);card.addEventListener("click",()=>selectManualPiece(piece));card.addEventListener("dragstart",e=>{draggingPiece=piece;e.dataTransfer.setData("text/plain",piece)});tray.appendChild(card)})}
function analyzeManual(){const targets=targetSet(),cellOwners=new Map(),invalidPieces=new Set();for(const p of Object.values(manualPlacements)){const cells=placementCells(p);if(!cells.length)invalidPieces.add(p.piece);for(const [r,c] of cells){const k=key(r,c);if(!LABELS.has(k)||targets.has(k))invalidPieces.add(p.piece);if(cellOwners.has(k)){invalidPieces.add(p.piece);invalidPieces.add(cellOwners.get(k))}else cellOwners.set(k,p.piece)}}return{valid:invalidPieces.size===0,invalidPieces,cellOwners}}
function renderManualBoard(){
  const b=$("manualBoard");
  b.innerHTML="";
  const analysis=analyzeManual();
  const targets=targetSet();
  const owners=new Map();
  for(const p of Object.values(manualPlacements)){
    for(const [r,c] of placementCells(p)) owners.set(key(r,c),p);
  }
  for(let r=0;r<7;r++){
    for(let c=0;c<7;c++){
      const k=key(r,c);
      const cell=document.createElement("div");
      cell.className="cell manual-cell";
      cell.dataset.row=r;
      cell.dataset.col=c;
      cell.style.gridRow=r+1;
      cell.style.gridColumn=c+1;
      if(!LABELS.has(k)){
        cell.classList.add("empty");
      }else{
        cell.textContent=LABELS.get(k);
        if(targets.has(k)) cell.classList.add("target");
        if(owners.has(k)){
          const pl=owners.get(k);
          cell.style.background=COLORS[pl.piece];
          cell.classList.add("occupied");
          if(analysis.invalidPieces.has(pl.piece)) cell.classList.add("invalid-placement");
          if(selectedManualPiece===pl.piece) cell.classList.add("selected-placement");
          const small=document.createElement("small");
          small.textContent=pl.piece;
          cell.appendChild(small);
          cell.draggable=true;
          cell.addEventListener("dragstart",e=>{
            draggingPiece=pl.piece;
            e.dataTransfer.setData("text/plain",pl.piece);
          });
          cell.addEventListener("click",()=>selectManualPiece(pl.piece));
        }else{
          cell.addEventListener("click",()=>{
            if(selectedManualPiece) placeManualPiece(selectedManualPiece,r,c);
          });
        }
        cell.addEventListener("dragover",e=>{
          e.preventDefault();
          cell.classList.add("drop-target");
        });
        cell.addEventListener("dragleave",()=>cell.classList.remove("drop-target"));
        cell.addEventListener("drop",e=>{
          e.preventDefault();
          cell.classList.remove("drop-target");
          const piece=e.dataTransfer.getData("text/plain")||draggingPiece;
          if(piece) placeManualPiece(piece,r,c);
        });
      }
      b.appendChild(cell);
    }
  }
  renderPieceTray();
  $("selectedPieceLabel").textContent=selectedManualPiece||t("noneSelected");
}
function selectManualPiece(piece){selectedManualPiece=piece;renderManualBoard()}
function placeManualPiece(piece,row,col,orientation=null){const previous=manualPlacements[piece]?{...manualPlacements[piece]}:null;const o=orientation??manualPlacements[piece]?.orientation??0;manualPlacements[piece]={piece,orientation:o,row,col};const analysis=analyzeManual();if(analysis.invalidPieces.has(piece)){if(previous)manualPlacements[piece]=previous;else delete manualPlacements[piece];setManualStatus(t("dropInvalid"),"error")}else setManualStatus(fmt(t("piecePlaced"),{piece}),"ok");selectedManualPiece=piece;currentSuggestion=null;$("suggestionBox").classList.add("hidden");$("manualCertifiedBadge").classList.add("hidden");renderManualBoard()}
function rotateSelected(){if(!selectedManualPiece)return;const p=manualPlacements[selectedManualPiece];if(!p){setManualStatus(t("manualReady"));return}const previous={...p};p.orientation=(p.orientation+1)%variants[p.piece].length;const analysis=analyzeManual();if(analysis.invalidPieces.has(p.piece)){manualPlacements[p.piece]=previous;setManualStatus(t("dropInvalid"),"error")}else setManualStatus(fmt(t("piecePlaced"),{piece:p.piece}),"ok");renderManualBoard()}
function removeSelected(){if(!selectedManualPiece)return;const p=selectedManualPiece;delete manualPlacements[p];setManualStatus(fmt(t("pieceRemoved"),{piece:p}));selectedManualPiece=null;$("manualCertifiedBadge").classList.add("hidden");renderManualBoard()}
function resetManual(){manualPlacements={};selectedManualPiece=null;currentSuggestion=null;$("suggestionBox").classList.add("hidden");$("manualCertifiedBadge").classList.add("hidden");setManualStatus(t("manualResetDone"));renderManualBoard()}
function setManualStatus(text,kind=""){const s=$("manualStatus");s.textContent=text;s.className=`manual-status ${kind}`}
function manualCertificate(){return PIECES.filter(p=>manualPlacements[p]).map(p=>manualPlacements[p])}
function checkManual(){const analysis=analyzeManual();$("manualCertifiedBadge").classList.add("hidden");if(!analysis.valid){setManualStatus(t("manualInvalid"),"error");renderManualBoard();return false}const cert=manualCertificate();if(cert.length<8){setManualStatus(t("manualPartialValid"),"ok");return true}const v=window.CalendarOCamlVerifier,valid=Boolean(v&&v.verify(current.month,current.day,cert));if(valid){const official=v?.engine==="rocq-extracted-ocaml"||v?.engine==="ocaml-js-of-ocaml";setManualStatus(official?t("manualComplete"):t("manualCompletePreview"),"ok");if(official)$("manualCertifiedBadge").classList.remove("hidden")}else setManualStatus(t("manualInvalid"),"error");return valid}
function showSuggestion(){currentSuggestion=null;const box=$("suggestionBox"),analysis=analyzeManual();box.classList.remove("hidden");$("suggestionPreview").innerHTML="";if(!current){$("suggestionText").textContent=t("suggestNoSolution");$("applySuggestionBtn").disabled=true;return}if(!analysis.valid){$("suggestionText").textContent=t("suggestInvalidFirst");$("applySuggestionBtn").disabled=true;return}const targetByPiece=Object.fromEntries(current.cert.map(p=>[p.piece,p]));for(const piece of PIECES){const placed=manualPlacements[piece],target=targetByPiece[piece];if(placed&&!samePlacement(placed,target)){currentSuggestion=target;$("suggestionText").innerHTML=fmt(t("suggestFix"),{piece,orientation:target.orientation,row:target.row,col:target.col,labels:coveredLabels(target)});$("suggestionPreview").appendChild(createShapeElement(piece,target.orientation,"large"));$("applySuggestionBtn").disabled=false;return}}for(const piece of PIECES){if(!manualPlacements[piece]){const target=targetByPiece[piece];currentSuggestion=target;$("suggestionText").innerHTML=fmt(t("suggestPlace"),{piece,orientation:target.orientation,row:target.row,col:target.col,labels:coveredLabels(target)});$("suggestionPreview").appendChild(createShapeElement(piece,target.orientation,"large"));$("applySuggestionBtn").disabled=false;return}}$("suggestionText").textContent=t("suggestDone");$("applySuggestionBtn").disabled=true}
function applySuggestion(){if(!currentSuggestion)return;const p=currentSuggestion;manualPlacements[p.piece]={...p};selectedManualPiece=p.piece;currentSuggestion=null;$("suggestionBox").classList.add("hidden");setManualStatus(fmt(t("piecePlaced"),{piece:p.piece}),"ok");renderManualBoard()}
async function solve(){
  await loadGuideForSelectedDate(false);
  if(!current){$("verifyMessage").textContent=t("noCertificate");return}
  $("certifiedBadge").classList.add("hidden");
  $("verifyMessage").textContent=t("loadingVerification");
  const v=window.CalendarOCamlVerifier;
  const valid=Boolean(v&&v.verify(current.month,current.day,current.cert));
  current.valid=valid;
  solutionRevealed=true;
  updateSelectedDate();
  $("certificate").textContent=certificateText(current.cert);
  if(valid&&(v?.engine==="rocq-extracted-ocaml"||v?.engine==="ocaml-js-of-ocaml")) $("certifiedBadge").classList.remove("hidden");
  updateVerificationMessage();
  renderSolutionBoard();
}

async function loadGuideForSelectedDate(resetManualBoard=true){
  const month=Number(monthSelect.value),day=Number(daySelect.value);
  const cert=solutionTable[`${String(month).padStart(2,"0")}-${String(day).padStart(2,"0")}`];
  solutionRevealed=false;
  guideStep=0;
  $("certifiedBadge").classList.add("hidden");
  $("certificate").textContent=t("notGenerated");
  $("verifyMessage").textContent=t("verificationPending");
  if(!cert){
    current=null;
    $("guideContent").classList.add("hidden");
    $("guideEmpty").classList.remove("hidden");
    renderSolutionBoard();
    return;
  }
  current={month,day,cert,valid:false};
  if(resetManualBoard) resetManual();
  updateSelectedDate();
  $("guideEmpty").classList.add("hidden");
  $("guideContent").classList.remove("hidden");
  updateGuide();
  renderManualBoard();
}

function changeLanguage(lang){currentLanguage=I18N[lang]?lang:"fr";localStorage.setItem("calendarPuzzleLanguage",currentLanguage);applyTranslations();fillMonths();fillDays();renderLegend();updateEngineStatus();renderPieceTray();renderManualBoard();if(current){updateSelectedDate();updateVerificationMessage();updateGuide()}else{$("selectedDate").textContent=t("chooseDate");$("verifyMessage").textContent=t("verificationPending");$("certificate").textContent=t("notGenerated")}setManualStatus(t("manualReady"))}
async function init(){
  applyTranslations();fillMonths();fillDays();renderLegend();renderSolutionBoard();renderManualBoard();
  setManualStatus(t("manualReady"));
  $("selectedDate").textContent=t("chooseDate");
  $("verifyMessage").textContent=t("verificationPending");
  $("certificate").textContent=t("notGenerated");
  $("certifiedBadge").classList.add("hidden");
  $("guideContent").classList.remove("hidden");
  $("guideEmpty").classList.add("hidden");
  solutionTable=await(await fetch("data/solutions.json")).json();
  updateEngineStatus();
  await loadGuideForSelectedDate(false);
}
function clearGeneratedSolution(){
  loadGuideForSelectedDate(true);
}
monthSelect.addEventListener("change",async()=>{fillDays();await clearGeneratedSolution()});daySelect.addEventListener("change",clearGeneratedSolution);$("language").addEventListener("change",e=>changeLanguage(e.target.value));$("solveBtn").addEventListener("click",solve);$("prevBtn").addEventListener("click",()=>{guideStep=Math.max(0,guideStep-1);updateGuide()});$("nextBtn").addEventListener("click",async()=>{if(!current) await loadGuideForSelectedDate(false);if(!current)return;guideStep=Math.min(8,guideStep+1);updateGuide()});$("showFuture").addEventListener("change",renderSolutionBoard);$("guidePlaceBtn").addEventListener("click",()=>{if(current&&guideStep>0){const p=current.cert[guideStep-1];manualPlacements[p.piece]={...p};selectedManualPiece=p.piece;renderManualBoard();setManualStatus(fmt(t("piecePlaced"),{piece:p.piece}),"ok")}});$("rotateManualBtn").addEventListener("click",rotateSelected);$("removeManualBtn").addEventListener("click",removeSelected);$("resetManualBtn").addEventListener("click",resetManual);$("checkManualBtn").addEventListener("click",checkManual);$("suggestBtn").addEventListener("click",showSuggestion);$("applySuggestionBtn").addEventListener("click",applySuggestion);$("copyBtn").addEventListener("click",async()=>{await navigator.clipboard.writeText($("certificate").textContent);$("copyBtn").textContent=t("copied");setTimeout(()=>$("copyBtn").textContent=t("copyCertificate"),1200)});init().catch(e=>{$("engineStatus").textContent=t("engineError");$("verifyMessage").textContent=String(e)});

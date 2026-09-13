# Resolve model roles before validating types. WLSMV does not turn a nominal
# predictor into an ordinal outcome. Keep the level order confirmed by the user.
sem_variable_spec <- function(d,pt,ordered_vars=character(),estimator='MLR') {
 vars<-lavaan::lavNames(pt,'ov')
 need(length(vars)>0&&all(vars%in%names(d)),'模型中的觀察變項不在資料中；請依資料字典使用變項名稱。')
 requested<-unique(ordered_vars %||% character())
 need(all(requested%in%vars),'另指定的順序題必須是模型中的觀察變項；請移除未納入模型的勾選。')
 exogenous<-lavaan::lavNames(pt,'ov.x')
 endogenous<-setdiff(vars,exogenous)
 automatic<-endogenous[vapply(d[endogenous],is.ordered,logical(1))]
 badx<-intersect(exogenous,union(requested,vars[!vapply(d[vars],is.numeric,logical(1))]))
 need(!length(badx),paste0(paste(badx,collapse='、'),' 是外生預測變項。二元／名目類別請先編碼虛擬變項；順序預測變項請先確認數值計分或編碼方式。WLSMV 的順序設定用於題項與內生結果。'))
 effective<-vars[vars%in%union(automatic,requested)]
 need(!length(effective)||identical(estimator,'WLSMV'),paste0('模型含順序題／結果：',paste(effective,collapse='、'),'。請將估計法改為 WLSMV；已確認的順序型態會自動套用，不需重複勾選。'))
 bad<-setdiff(vars[!vapply(d[vars],is.numeric,logical(1))],effective)
 need(!length(bad),paste0(paste(bad,collapse='、'),' 尚未指定類別順序。若為二元或順序題／結果，請到「資料與變項 → 修改」確認順序型態及級別順序，或在「另指定順序題」確認採用目前順序；多類別名目結果不適用此 WLSMV 模型。'))
 level_order<-lapply(effective,function(v)if(is.factor(d[[v]]))levels(d[[v]])else levels(ordered(d[[v]])))
 names(level_order)<-effective
 tab<-data.frame(Variable=vars,Model_role=ifelse(vars%in%exogenous,'外生預測變項','測量題項／內生結果'),
  Input_type=vapply(d[vars],function(a)if(is.ordered(a))'順序'else if(is.factor(a)||is.character(a))'類別'else'數值',character(1)),
  Treatment=ifelse(vars%in%effective,'順序類別','數值'),
  Source=ifelse(vars%in%automatic,'資料型態自動套用',ifelse(vars%in%requested,'使用者另指定','原數值')),
  Level_order=vapply(vars,function(v)paste(level_order[[v]],collapse=' < '),character(1)),row.names=NULL)
 list(variables=vars,ordered=effective,automatic=automatic,requested=requested,levels=level_order,table=tab)
}

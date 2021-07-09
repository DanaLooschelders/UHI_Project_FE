#calculate AOA
test_aoa<-aoa(pred_stack, model_ffs)
test_aoa$AOA[test_aoa$AOA==1]
spplot(test_aoa, zcol="AOA")

spplot(ratify(test_aoa$AOA), col.regions=c("black","white"))

ncell(test_aoa$AOA[test_aoa$AOA==1])/ncell(test_aoa$AOA)*100

spplot(test_aoa_factor$AOA)
confusionMatrix(factor(model_ffs$pred),factor(model_ffs$pred))

#plot areas inside AOA
test_predict2<-test_predict[test_aoa$AOA == 0] <- NA
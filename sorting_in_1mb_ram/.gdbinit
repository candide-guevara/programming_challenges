define pbucket
  ignore-errors p { (void*)buckets.starts[$arg0], (void*)buckets.ends[$arg0] }
  ignore-errors p { buckets.lens[$arg0]/8, buckets.lens[$arg0]%8 }
  ignore-errors p (void*)buckets.starts[$arg0] + buckets.lens[$arg0]/8
  ignore-errors p { (void*)starts[$arg0], (void*)ends[$arg0] }
  ignore-errors p { lens[$arg0]/8, lens[$arg0]%8 }
  ignore-errors p (void*)starts[$arg0] + lens[$arg0]/8
end


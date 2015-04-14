#!/bin/bash 


# lists obtained using:
# readelf -s bzip2 |grep FUNC | awk -F '[[:space:]]+' '{printf "%s\n, ", $9}' | cut -f1 -d"@" | c++filt | awk '{printf "%s", $0}'
# nm --format='sysv' h264ref | ./cm.py 
#main has been removed from BZIP_FUNCS
BZIP_FUNCS='add_pair_to_block,atoi@@GLIBC_2.2.5,bsFinishWrite,bsPutUChar,bsPutUInt32,bsW,BZ2_blockSort,BZ2_bsInitWrite,BZ2_bz__AssertH__fail,BZ2_bzBuffToBuffCompress,BZ2_bzBuffToBuffDecompress,BZ2_bzclose,BZ2_bzCompress,BZ2_bzCompressEnd,BZ2_bzCompressInit,BZ2_bzDecompress,BZ2_bzDecompressEnd,BZ2_bzDecompressInit,BZ2_bzdopen,BZ2_bzerror,BZ2_bzflush,BZ2_bzlibVersion,BZ2_bzopen,BZ2_bzread,BZ2_bzRead,BZ2_bzReadClose,BZ2_bzReadGetUnused,BZ2_bzReadOpen,BZ2_bzwrite,BZ2_bzWrite,BZ2_bzWriteClose,BZ2_bzWriteClose64,BZ2_bzWriteOpen,BZ2_compressBlock,BZ2_decompress,BZ2_hbAssignCodes,BZ2_hbCreateDecodeTables,BZ2_hbMakeCodeLengths,BZ2_indexIntoF,bz_config_ok,bzopen_or_bzdopen,cadvise,cleanUpAndFail,close@@GLIBC_2.2.5,compressedStreamEOF,compressStream,configError,copy_input_until_stop,copy_output_until_stop,crcError,__ctype_b_loc@@GLIBC_2.3,__cyg_profile_func_enter,__cyg_profile_func_exit,debug_time,default_bzalloc,default_bzfree,deregister_tm_clones,__do_global_dtors_aux,__errno_location@@GLIBC_2.2.5,exit@@GLIBC_2.2.5,fallbackQSort3,fallbackSimpleSort,fallbackSort,_fini,flush_RL,__fprintf_chk@@GLIBC_2.3.4,fprintf@@GLIBC_2.2.5,fputc@@GLIBC_2.2.5,frame_dummy,free@@GLIBC_2.2.5,fwrite@@GLIBC_2.2.5,generateMTFValues,handle_compress,_init,init_RL,ioError,isempty_RL,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,main,mainGtU,mainQSort3,mainSimpleSort,mainSort,makeMaps_d,makeMaps_e,malloc@@GLIBC_2.2.5,memcpy@@GLIBC_2.14,memset@@GLIBC_2.2.5,mmed3,myfeof,myfeof,open64@@GLIBC_2.2.5,outOfMemory,panic,perror@@GLIBC_2.2.5,prepare_new_block,__printf_chk@@GLIBC_2.3.4,printf@@GLIBC_2.2.5,puts@@GLIBC_2.2.5,ran,read@@GLIBC_2.2.5,register_tm_clones,sendMTFValues,setExit,showFileNames,spec_compress,spec_fread,spec_fwrite,spec_getc,spec_init,spec_initbufs,spec_load,spec_putc,spec_random_load,spec_read,spec_reset,spec_rewind,spec_uncompress,spec_ungetc,spec_write,__stack_chk_fail@@GLIBC_2.4,_start,strcat@@GLIBC_2.2.5,strerror@@GLIBC_2.2.5,strtol@@GLIBC_2.2.5,uInt64_from_UInt32s,uInt64_isZero,uInt64_qrm10,uInt64_toAscii,uInt64_to_double,uncompressStream,unRLE_obuf_to_output_FAST,unRLE_obuf_to_output_SMALL'
BLACKSCHOLES_FUNCS='atoi@@GLIBC_2.2.5,__cyg_profile_func_enter,__cyg_profile_func_exit,deregister_tm_clones,__do_global_dtors_aux,exit@@GLIBC_2.2.5,exp@@GLIBC_2.2.5,fclose@@GLIBC_2.2.5,fflush@@GLIBC_2.2.5,_fini,fopen@@GLIBC_2.2.5,fprintf@@GLIBC_2.2.5,frame_dummy,free@@GLIBC_2.2.5,fscanf@@GLIBC_2.2.5,__gxx_personality_v0@@CXXABI_1.3,_init,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,log@@GLIBC_2.2.5,main,malloc@@GLIBC_2.2.5,printf@@GLIBC_2.2.5,pthread_create@@GLIBC_2.2.5,pthread_join@@GLIBC_2.2.5,pthread_mutexattr_init@@GLIBC_2.2.5,puts@@GLIBC_2.2.5,register_tm_clones,sqrt@@GLIBC_2.2.5,_start,_Unwind_Resume@@GCC_3.0,BlkSchlsEqEuroNoDiv(float, float, float, float, float, int, float),CNDF(float),bs_thread(void*)'
FLUID_FUNCS='__assert_fail@@GLIBC_2.2.5,atoi@@GLIBC_2.2.5,__cxa_atexit@@GLIBC_2.2.5,__cyg_profile_func_enter,__cyg_profile_func_exit,deregister_tm_clones,__do_global_dtors_aux,exit@@GLIBC_2.2.5,_fini,fprintf@@GLIBC_2.2.5,frame_dummy,free@@GLIBC_2.2.5,_GLOBAL__sub_I_pools,_GLOBAL__sub_I__Z13cellpool_initP8cellpooli,__gxx_personality_v0@@CXXABI_1.3,_init,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,main,memset@@GLIBC_2.2.5,posix_memalign@@GLIBC_2.2.5,powf@@GLIBC_2.2.5,pthread_attr_destroy@@GLIBC_2.2.5,pthread_attr_init@@GLIBC_2.2.5,pthread_attr_setdetachstate@@GLIBC_2.2.5,pthread_cond_broadcast@@GLIBC_2.3.2,pthread_cond_destroy@@GLIBC_2.3.2,pthread_cond_init@@GLIBC_2.3.2,pthread_cond_wait@@GLIBC_2.3.2,pthread_create@@GLIBC_2.2.5,pthread_join@@GLIBC_2.2.5,pthread_mutex_destroy@@GLIBC_2.2.5,pthread_mutex_init@@GLIBC_2.2.5,pthread_mutex_lock@@GLIBC_2.2.5,pthread_mutex_trylock@@GLIBC_2.2.5,pthread_mutex_unlock@@GLIBC_2.2.5,register_tm_clones,sqrtf@@GLIBC_2.2.5,__stack_chk_fail@@GLIBC_2.4,_start,_Unwind_Resume@@GCC_3.0,CleanUpSim(),cellpool_init(cellpool*, int),RebuildGridMT(int),AdvanceFrameMT(int),AdvanceFramesMT(void*),ComputeForcesMT(int),cellpool_destroy(cellpool*),cellpool_getcell(cellpool*),ClearParticlesMT(int),InitNeighCellList(int, int, int, int*),AdvanceParticlesMT(int),ComputeDensitiesMT(int),cellpool_returncell(cellpool*, Cell*),ComputeDensities2MT(int),parsec_barrier_init(parsec_barrier_t*, int const*, unsigned int),parsec_barrier_wait(parsec_barrier_t*),ProcessCollisionsMT(int),ProcessCollisions2MT(int),parsec_barrier_destroy(parsec_barrier_t*),parsec_barrierattr_init(int*),InitDensitiesAndForcesMT(int),parsec_barrierattr_destroy(int*),parsec_barrierattr_getpshared(int const*, int*),parsec_barrierattr_setpshared(int*, int),__static_initialization_and_destruction_0(int, int),__static_initialization_and_destruction_0(int, int),InitSim(char const*, unsigned int),SaveFile(char const*),hmgweight(unsigned int, int*),operator delete[](void*)@@GLIBCXX_3.4,bswap_float(float),bswap_int32(int),isLittleEndian(),Not_Implemented(char const*, char const*, unsigned int),cellpool_allocblock(int),Cell::Cell(),Cell::Cell(),Vec3::Vec3(float, float, float),Vec3::Vec3(),Vec3::Vec3(float, float, float),Vec3::Vec3(),Vec3::operator/=(float),Vec3::operator-=(Vec3 const&),Vec3::operator*=(float),Vec3::operator+=(Vec3 const&),operator new[](unsigned long)@@GLIBCXX_3.4,Vec3::GetLengthSq() const,Vec3::operator-(Vec3 const&) const,Vec3::operator*(float) const,Vec3::operator+(Vec3 const&) const,std::basic_ios<char, std::char_traits<char> >::operator void*() const@@GLIBCXX_3.4,std::basic_ios<char, std::char_traits<char> >::operator!() const@@GLIBCXX_3.4,std::basic_istream<char, std::char_traits<char> >::read(char*, long)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::write(char const*, long)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(float)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(int)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(std::basic_ostream<char, std::char_traits<char> >& (*)(std::basic_ostream<char, std::char_traits<char> >&))@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::basic_ifstream(char const*, std::_Ios_Openmode)@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::~basic_ifstream()@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::basic_ofstream(char const*, std::_Ios_Openmode)@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::~basic_ofstream()@@GLIBCXX_3.4,std::ios_base::Init::Init()@@GLIBCXX_3.4,std::ios_base::Init::~Init()@@GLIBCXX_3.4,operator new(unsigned long, void*),float const& std::max<float>(float const&, float const&),std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@@GLIBCXX_3.4,void std::swap<Cell*>(Cell*&, Cell*&),void std::swap<int*>(int*&, int*&),std::basic_ostream<char, std::char_traits<char> >& std::flush<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@@GLIBCXX_3.4'
H264_FUNCS='adaptive_memory_management,AddUpSADQuarter,alloc_colocated,alloc_frame_store,AllocNalPayloadBuffer,AllocNALU,AllocPPS,alloc_ref_pic_list_reordering_buffer,AllocSPS,alloc_storable_picture,AppendTmpbits2Buf,arienco_bits_written,arienco_create_encoding_environment,arienco_delete_encoding_environment,arienco_done_encoding,arienco_start_encoding,__assert_fail@@GLIBC_2.2.5,B8Mode2Value,biari_encode_symbol,biari_encode_symbol_eq_prob,biari_encode_symbol_final,biari_init_context,BIDPartitionCost,BlockMotionSearch,BPredPartitionCost,BType2CtxRef,buf2img,Build_Status_Map,cabac_new_slice,calc_buffer,calc_MAD,CalculateFrameNumber,CalculateOffset8Param,CalculateOffsetParam,CalculateQuant8Param,CalculateQuantParam,CalculateSparePicture,calloc@@GLIBC_2.2.5,CAVLC_init,cbp_linfo_inter,cbp_linfo_intra,ceil@@GLIBC_2.2.5,CeilLog2,CheckAvailabilityOfNeighbors,CheckAvailabilityOfNeighborsCABAC,CheckOffsetParameterName,CheckParameterName,CheckReliabilityOfRef,ChromaPrediction4x4,ChromaResidualCoding,ClearFastFullIntegerSearch,clear_gop_structure,Clear_Motion_Search_Module,ClearPanScanRectInfoPayload,clear_picture,ClearRandomAccess,clear_rdopt,clear_sei_message,ClearSubseqCharPayload,ClearSubseqInfoPayload,ClearUser_data_registered_itu_t_t35,ClearUser_data_unregistered,clip1a,clip1a_chr,CloseAnnexbFile,close@@GLIBC_2.2.5,ClosePanScanRectInfo,CloseRandomAccess,CloseRTPFile,CloseSceneInformation,CloseSEIMessages,CloseSparePicture,CloseSubseqChar,CloseSubseqInfo,CloseSubseqLayerInfo,CloseUser_data_registered_itu_t_t35,CloseUser_data_unregistered,code_a_picture,combine_field,compare_fs_by_frame_num_desc,compare_fs_by_lt_pic_idx_asc,compare_fs_by_poc_asc,compare_fs_by_poc_desc,compare_pic_by_lt_pic_num_asc,compare_pic_by_pic_num_desc,compare_pic_by_poc_asc,compare_pic_by_poc_desc,ComposeRTPPacket,ComposeSparePictureMessage,CompressSpareMBMap,compute_colocated,ComputeFrameMAD,compute_residue_b8block,compute_residue_mb,Conceal_Error,Configure,copyblock4x4,copyblock_sp,copy_motion_vectors_MB,copy_params,copy_rdopt_data,create_coding_state,create_context_memory,create_contexts_MotionInfo,create_contexts_TextureInfo,create_pyramid,__ctype_b_loc@@GLIBC_2.3,__cyg_profile_func_enter,__cyg_profile_func_exit,dct_chroma,dct_chroma4x4,dct_chroma_DC,dct_chroma_sp,dct_luma,dct_luma_16x16,dct_luma8x8,dct_luma_sp,DeblockFrame,DeblockMb,decide_fld_frame,decide_intrabk_SAD,decode_one_b8block,decode_one_mb,DecOneForthPix,dec_ref_pic_marking,DefineThreshold,DefineThresholdMB,delete_coding_state,delete_contexts_MotionInfo,delete_contexts_TextureInfo,deregister_tm_clones,direct_output,DisplayEncoderParams,distortion_fld,__do_global_dtors_aux,dpb_combine_field,dpb_split_field,dummy_slice_too_big,dump_dpb,EdgeLoop,encode_enhancement_layer,encode_one_frame,encode_one_macroblock,encode_one_slice,error,Error_Concealment,estimate_weighting_factor_B_slice,estimate_weighting_factor_P_slice,exit@@GLIBC_2.2.5,exp_golomb_encode_eq_prob,FastFullPelBlockMotionSearch,FastIntegerPelBlockMotionSearch,FastLine16Y_11,FastLineX,FastPelY_14,FastSubPelBlockMotionSearch,fclose@@GLIBC_2.2.5,field_flag_inference,field_mode_buffer,field_picture,fill_frame_num_gap,FinalizePanScanRectInfo,FinalizeRandomAccess,FinalizeSceneInformation,finalize_sei_message,FinalizeSpareMBMap,FinalizeSubseqChar,FinalizeSubseqInfo,FinalizeSubseqLayerInfo,FinalizeUser_data_registered_itu_t_t35,FinalizeUser_data_unregistered,find_distortion,find_sad_16x16,find_SATD,FindSkipModeMotionVector,find_snr,_fini,floor@@GLIBC_2.2.5,flush_direct_output,flush_dpb,FmoEndPicture,FmoGenerateMapUnitToSliceGroupMap,FmoGenerateMBAmap,FmoGenerateType0MapUnitMap,FmoGenerateType1MapUnitMap,FmoGenerateType2MapUnitMap,FmoGenerateType3MapUnitMap,FmoGenerateType4MapUnitMap,FmoGenerateType5MapUnitMap,FmoGenerateType6MapUnitMap,FmoGetFirstMacroblockInSlice,FmoGetFirstMBOfSliceGroup,FmoGetLastCodedMBOfSliceGroup,FmoGetNextMBNr,FmoGetPreviousMBNr,FmoInit,FmoMB2SliceGroup,FmoSetLastMacroblockInSlice,FmoSliceGroupCompletelyCoded,FmoStartPicture,FmoUninit,fopen@@GLIBC_2.2.5,fprintf@@GLIBC_2.2.5,fputc@@GLIBC_2.2.5,frame_dummy,frame_mode_buffer,frame_picture,fread@@GLIBC_2.2.5,free_colocated,free_context_memory,free_dpb,free_frame_store,free@@GLIBC_2.2.5,free_global_buffers,free_img,free_mem2D,free_mem2Dint,free_mem2Dint64,free_mem2Dpel,free_mem2Dshort,free_mem3D,free_mem3Dint,free_mem3Dint64,free_mem3Dpel,free_mem3Dshort,free_mem4Dint,free_mem4Dshort,free_mem_ACcoeff,free_mem_bwmincost,free_mem_DCcoeff,free_mem_FME,free_mem_mincost,free_mem_mv,FreeNalPayloadBuffer,FreeNALU,free_orig_planes,FreeParameterSets,free_picture,FreePPS,free_ref_pic_list_reordering_buffer,free_slice,free_slice_list,FreeSPS,free_storable_picture,free_top_bot_planes,fseek@@GLIBC_2.2.5,ftell@@GLIBC_2.2.5,FullPelBlockMotionBiPred,FullPelBlockMotionSearch,fwrite@@GLIBC_2.2.5,GenerateFullPelRepresentation,GenerateParameterSets,GeneratePic_parameter_set_NALU,GeneratePic_parameter_set_rbsp,GeneratePictureParameterSet,GenerateSeq_parameter_set_NALU,GenerateSeq_parameter_set_rbsp,GenerateSequenceParameterSet,GenerateVUISequenceParameters,gen_field_ref_ids,gen_pic_list_from_frame_list,getAffNeighbour,GetBestTransformP8x8,getChroma4x4Neighbour,GetConfigFileContent,GetCtxModelNumber,Get_Direct_Cost8x8,Get_Direct_CostMB,Get_Direct_Motion_Vectors,getDpbSize,get_LeakyBucketRate,get_long_term_pic,getLuma4x4Neighbour,get_mb_block_pos,get_mb_pos,get_mem2D,get_mem2Dint,get_mem2Dint64,get_mem2Dpel,get_mem2Dshort,get_mem3D,get_mem3Dint,get_mem3Dint64,get_mem3Dpel,get_mem3Dshort,get_mem4Dint,get_mem4Dshort,get_mem_ACcoeff,get_mem_bwmincost,get_mem_DCcoeff,get_mem_FME,get_mem_mincost,get_mem_mv,getNeighbour,getNonAffNeighbour,get_pic_num_x,get_picture_type,Get_Reference_Block,Get_Reference_Pixel,get_short_term_pic,GetSkipCostMB,get_smallest_poc,GetStrength,gop_pyramid,HaveAggregationSEI,I16Offset,IdentifyLevel,IdentifyProfile,idr_memory_management,img2buf,information_init,_init,init_contexts,init_dec_ref_pic_marking_buffer,init_dpb,InitEncoderParams,init_field,init_frame,init_global_buffers,init_gop_structure,InitializeFastFullIntegerSearch,init_img,init_lists,init_mbaff_lists,Init_Motion_Search_Module,init_orig_buffers,init_out_buffer,InitPanScanRectInfo,init_poc,Init_QMatrix,Init_QOffsetMatrix,InitRandomAccess,init_rdopt,init_ref_pic_list_reordering,InitSceneInformation,InitSEIMessages,init_slice,InitSparePicture,InitSubseqChar,InitSubseqInfo,InitSubseqLayerInfo,init_top_bot_planes,InitUser_data_registered_itu_t_t35,InitUser_data_unregistered,insert_picture_in_dpb,interpret_gop_structure,Intra16x16_Mode_Decision,IntraChromaPrediction,IntraChromaPrediction4x4,intrapred_luma,intrapred_luma_16x16,intrapred_luma8x8,is_long_ref,is_long_term_reference,__isoc99_fscanf@@GLIBC_2.7,__isoc99_sscanf@@GLIBC_2.7,is_short_ref,is_short_term_reference,is_used_for_reference,JMHelpExit,LevelCheck,levrun_linfo_c2x2,levrun_linfo_inter,levrun_linfo_intra,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,localtime@@GLIBC_2.2.5,log10@@GLIBC_2.2.5,log@@GLIBC_2.2.5,LowPassForIntra8x8Pred,lseek@@GLIBC_2.2.5,LumaPrediction4x4,LumaPrediction4x4Bi,LumaResidualCoding,LumaResidualCoding8x8,MADModelEstimator,main,malloc@@GLIBC_2.2.5,malloc_picture,malloc_slice,mark_pic_long_term,MbAffPostProc,mb_is_available,MBType2Value,memcpy@@GLIBC_2.14,memset@@GLIBC_2.2.5,mm_assign_long_term_frame_idx,mm_mark_current_picture_long_term,mm_unmark_all_long_term_for_reference,mm_unmark_all_short_term_for_reference,mm_unmark_long_term_for_reference,mm_unmark_short_term_for_reference,mm_update_max_long_term_frame_idx,Mode_Decision_for_4x4IntraBlocks,Mode_Decision_for_8x8IntraBlocks,Mode_Decision_for_Intra4x4Macroblock,Mode_Decision_for_new_8x8IntraBlocks,Mode_Decision_for_new_Intra8x8Macroblock,modify_redundant_pic_cnt,no_mem_exit,OneComponentChromaPrediction4x4,OneComponentLumaPrediction4x4,OpenAnnexbFile,open@@GLIBC_2.2.5,OpenRTPFile,output_one_frame_from_dpb,PaddAutoCropBorders,ParameterNameToMapIndex,ParseContent,ParseMatrix,ParseQOffsetMatrix,PartCalMad,Partition_BC_Header,PartitionMotionSearch,PatchInp,PatchInputNoFrames,PatchMatrix,picture_coding_decision,picture_structure_decision,poc_based_ref_management,poc_ref_pic_reorder,pow@@GLIBC_2.2.5,predict_nnz,predict_nnz_chroma,pred_weight_table,printf@@GLIBC_2.2.5,proceed2nextMacroblock,process_2nd_IGOP,ProfileCheck,PutBigDoubleWord,put_buffer_bot,put_buffer_frame,put_buffer_top,putchar@@GLIBC_2.2.5,PutPel_11,PutPel_14,puts@@GLIBC_2.2.5,QP2Qstep,qsort@@GLIBC_2.2.5,Qstep2QP,RandomIntra,RandomIntraInit,RandomIntraNewPicture,RandomIntraUninit,RBSPtoEBSP,RBSPtoNALU,rc_alloc,rc_free,rc_init_GOP,rc_init_pict,rc_init_seq,RCModelEstimator,rc_update_pict,rc_update_pict_frame,RDCost_for_4x4Blocks_Chroma,RDCost_for_4x4IntraBlocks,RDCost_for_8x8blocks,RDCost_for_8x8IntraBlocks,RDCost_for_macroblocks,rd_pic_decision,rdPictureCoding,read@@GLIBC_2.2.5,ReadOneFrame,ref_pic_list_reordering,register_tm_clones,remove_frame_from_dpb,remove_unused_frame_from_dpb,reorder_long_term,reorder_ref_pic_list,reorder_short_term,replace_top_pic_with_frame,report,ReportB,ReportFirstframe,report_frame_statistic,ReportIntra,ReportNALNonVLCBits,ReportP,ReportRB,ReportSP,report_stats_on_error,reset_coding_state,reset_coding_state_cs_cm,ResetFastFullIntegerSearch,RestoreMV8x8,RestoreMVBlock8x8,RTPUpdateTimestamp,SATD,SATD8X8,Scaling_List,se_linfo,setbitscount,SetCoeffAndReconstruction8x8,SetCtxModelNumber,SetImgType,set_last_dquant,set_mbaff_parameters,set_MB_parameters,SetModesAndRefframe,SetModesAndRefframeForBlocks,SetMotionVectorPredictor,SetMotionVectorsMB,SetRefAndMotionVectors,set_ref_pic_num,set_stored_macroblock_parameters,SetupFastFullPelSearch,SetupLargerBlocks,se_v,sign,skip_intrabk_SAD,SliceHeader,slice_too_big,sliding_window_memory_management,snprintf@@GLIBC_2.2.5,SODBtoRBSP,Sort,spec_rand,spec_srand,sqrt@@GLIBC_2.2.5,__stack_chk_fail@@GLIBC_2.4,_start,start_macroblock,start_sequence,start_slice,store_coding_state,store_coding_state_cs_cm,store_contexts,store_macroblock_parameters,StoreMV8x8,StoreMVBlock8x8,StoreNewMotionVectorsBlock8x8,store_picture_in_dpb,strcmp@@GLIBC_2.2.5,strftime@@GLIBC_2.2.5,strlen@@GLIBC_2.2.5,strncat@@GLIBC_2.2.5,strncmp@@GLIBC_2.2.5,strncpy@@GLIBC_2.2.5,SubPelBlockMotionSearch,SubPelBlockSearchBiPred,symbol2uvlc,symbol2vlc,terminate_macroblock,terminate_sequence,terminate_slice,TestEncoderParams,testEndian,test_wp_B_slice,test_wp_P_slice,time@@GLIBC_2.2.5,TransformDecision,u_1,ue_linfo,ue_v,UMVLine16Y_11,UMVLineX,UMVPelY_14,unary_bin_encode,unary_bin_max_encode,unary_exp_golomb_level_encode,unary_exp_golomb_mv_encode,UnifiedOneForthPix,uninit_out_buffer,unmark_for_long_term_reference,unmark_for_reference,unmark_long_term_field_for_reference_by_frame_idx,unmark_long_term_frame_for_reference_by_frame_idx,UpdateDecoders,update_field_frame_contexts,update_ltref_list,updateMADModel,UpdatePanScanRectInfo,UpdatePixelMap,updateQuantizationParameter,UpdateRandomAccess,updateRCModel,update_ref_list,UpdateSceneInformation,UpdateSubseqChar,UpdateSubseqInfo,UpdateUser_data_registered_itu_t_t35,UpdateUser_data_unregistered,u_v,write_and_store_CBP_block_bit,WriteAnnexbNALU,writeB8_typeInfo_CABAC,write_buffer,writeCBPandLumaCoeff,writeCBP_BIT_CABAC,writeCBP_CABAC,writeChromaCoeff,writeChromaIntraPredMode,writeCIPredMode_CABAC,writeCoeff4x4_CAVLC,writeDquant_CABAC,writeFieldModeInfo_CABAC,write@@GLIBC_2.2.5,writeIntra4x4Modes,writeIntraPredMode_CABAC,writeLumaCoeff4x4_CABAC,writeLumaCoeff8x8,writeLumaCoeff8x8_CABAC,writeMBLayer,writeMB_skip_flagInfo_CABAC,writeMB_transform_size_CABAC,writeMB_typeInfo_CABAC,writeMotionInfo2NAL,writeMotionVector8x8,writeMVD_CABAC,write_one_macroblock,write_out_picture,writeout_picture,write_picture,write_PPS,writeReferenceFrame,writeRefFrame_CABAC,WriteRTPNALU,WriteRTPPacket,writeRunLevel_CABAC,write_sei_message,write_significance_map,write_significant_coefficients,write_stored_frame,writeSyntaxElement2Buf_Fixed,writeSyntaxElement2Buf_UVLC,writeSyntaxElement_CABAC,writeSyntaxElement_fixed,writeSyntaxElement_Intra4x4PredictionMode,writeSyntaxElement_Level_VLC1,writeSyntaxElement_Level_VLCN,writeSyntaxElement_NumCoeffTrailingOnes,writeSyntaxElement_NumCoeffTrailingOnesChromaDC,writeSyntaxElement_Run,writeSyntaxElement_TotalZeros,writeSyntaxElement_TotalZerosChromaDC,writeSyntaxElement_UVLC,writeSyntaxElement_VLC,write_terminating_bit,writeUnit,write_unpaired_field,writeUVLC2buffer,writeVlcByteAlign,XRate,ZeroRef'
HMMER_FUNCS='ctime,exp,ftell,abort,__fprintf_chk,puts,fseek,exit,__printf_chk,putchar,strncmp,__libc_start_main,system,__cyg_profile_func_enter,__ctype_toupper_loc,fputc,free,strlen,__vfprintf_chk,__ctype_b_loc,floorf,strrchr,pow,strstr,rewind,fputs,strtol,__strcpy_chk,strspn,strchr,getenv,__cyg_profile_func_exit,__stack_chk_fail,strcmp,__fread_chk,strtok,__ctype_tolower_loc,calloc,feof,fclose,remove,__sprintf_chk,strcspn,fopen64,fwrite,realloc,floor,perror,localtime,strftime,sqrt,strtoul,__strncat_chk,log,logf,strtod,time,fflush,sqrtf,fread,strcpy,memset,_fini,atof,strncpy,_init,atoi,vfprintf,fprintf,fgets,tolower,strcat,strncat,memmove,memcpy,printf,sprintf,__strpbrk_c3,main_loop_serial,deregister_tm_clones,register_tm_clones,__do_global_dtors_aux,frame_dummy,get_wee_midpt,qst,compare_lists,make_alilist,make_ref_alilist,main_loop_serial,init_ilogsum,copy_alignment_line,actually_write_selex,parse_gf,parse_gs,parse_sequence,parse_gc,parse_gr,parse_comment,actually_write_stockholm,read_asc10hmm,read_asc11hmm,byteswap,ascii2prob,read_bin_string,read_bin20hmm,read_plan9_binhmm,read_bin10hmm,read_bin11hmm,read_bin17hmm,read_bin19hmm,read_plan9_aschmm,read_asc17hmm,read_asc19hmm,read_asc20hmm,prob2ascii,write_bin_string,multiline,upweight,downweight,simple_distance,simple_diffmx,set_degenerate,trace_doctor,fake_tracebacks,annotate_model,matassign2hmm,build_cij,estimate_model_length,score2postcode,gki_hashvalue,gki_alloc,gki_upsize,regc,regnode,regnext,regrepeat,regmatch,regtry,reginsert,regtail,regoptail,reg,regbranch,regatom,regpiece,default_amino_prior,default_nucleic_prior,gsi_keysorter,endPearson,endZuker,endGCGdata,SeqfileGetLine,endIG,endStrider,endGB,addseq,readLoop,readIG,readPearson,readZuker,readUWGCG,readStrider,readGenBank,readEMBL,endEMBL,readPIR,endPIR,readGCGdata,seqfile_open,clear_ssifile,current_index_size,read_i16,read_i64,read_i32,read_offset,skeysort,pkeysort,indexfile_position,binary_search,load_indexfile,activate_external_sort,write_i64,parse_skey_info,parse_pkey_info,write_i16,write_i32,write_offset,rightjustify,GaussianFitHistogram,Strparse,P7SmallViterbiSize,sre_random_positive,ctime,MSAShorterAlignment,P7ReallocTrace,CompareMultAlignments,ReadSELEX,exp,WriteBinHMM,FreePhylo,MSAGetSeqAccession,SeqfilePosition,PrintTransition,GSIClose,printf,sre_hton16,PValue,MakeDiffMx,EnvFileOpen,Scorify,Warn,GSIFreeIndex,TransitionScoreLookup,ImposeMasterTrace,TraceVerify,InitIntStack,EVDDensity,MSAAddComment,memset,ExtremeValueP,ftell,FMin,__libc_csu_fini,InitAinfo,HMMFilePositionByName,Free2DArray,TraceSet,SeqfileLineParameters,DExp,_start,ToDNA,DAdd,StateOccupancy,PositionBasedWeights,ReadMSF,PostalCode,SampleDirichlet,CreatePlan7Matrix,GCGMultchecksum,Plan7SetAccession,PrintFancyAli,abort,HMMERBanner,P7PrintTrace,MSAFileRead,strncat,ReadPhylip,MSAGetSeqDescription,P7AllocPrior,s2lower,__fprintf_chk,FreeShadowMatrix,HMMFilePositionByIndex,Strinsert,Getword,FScale,FExp,Strdup,puts,SetAutocuts,SingleLinkCluster,fseek,TophitsReport,FreeFancyAli,TraceDomainNumber,Plan7GlobalConfig,toupper,SeqfileFormat,StrDPShuffle,exit,MSASetSeqAccession,__printf_chk,AlignmentIdentityBySampli,P7FreePrior,P7WeeViterbi,P7LaplacePrior,ILogsum,revcomp,ZeroPlan7,P7TraceCount,_fini,SSIAddSecondaryKeyToIndex,GSIAllocIndex,GKIStoreKey,MajorityRuleConsensus,PrintXMGRDistribution,putchar,Seqtype,Linefit,P7Backward,P7ReadPrior,WeightedLinefit,MSAAlloc,sre_strncpy,strncmp,malloc,StrMarkov0,__libc_start_main,SeqfileClose,P9DefaultNullModel,TraceDecompose,CompareRefPairAlignments,system,MSAFileGetLine,FMax,DegenerateSymbolScore,IsReal,Die,WriteMSF,P7OptimalAccuracyTrace,AllocAlignment,GrowTophits,IncompleteGamma,FCopy,DealignAseqs,ReadClustal,P7Handmodelmaker,WriteStockholm,PostprocessSignificantHit,SeqfileOpenForIndexing,AllocFancyAli,GSIWriteFileRecord,AddToHistogram,P7PriorifyEmissionVector,MSASmallerAlignment,P7DefaultNullModel,Translate,Plan7SetName,RegisterHit,PAMPrior,sqd_regcomp,VoronoiWeights,DLogSum,IsSELEXFormat,GCGchecksum,DScale,HMMFileRewind,DefaultGeneticCode,P7PrintPrior,EmitConsensusSequence,Getline,WriteSimpleFASTA,P7AllocTrace,MSASetSeqDescription,MSAFileFormat,MSAAddGF,P7ParsingViterbi,FMX2Free,DisplayPlan7PostAlign,IsBlankline,ExponentialRandom,AllocShadowMatrix,WriteSeq,fgets,sre_fgets,__cyg_profile_func_enter,PairwiseIdentity,P7PriorifyHMM,GaussianSetHistogram,atof,MingapAlignment,sre_strdup,AllocPlan7Matrix,vfprintf,MSAGetSeqidx,FileDirname,GSIWriteKeyRecord,DDot,SymbolIndex,ReadSeq,Gaussrandom,__ctype_toupper_loc,MSANogap,DefaultCodonBias,fputc,Plan7SWConfig,QRNAShuffle,MSAFree,FDot,P7FillOptimalAccuracy,GSIWriteHeader,RandomSequence,GCGBinaryToSequence,FileTail,free,SSIFileInfo,SetAlphabet,strlen,SeqfileFormat2String,PrintPhylo,GKIKeyIndex,Prob2Score,sre_strcat,MSAAverageSequenceLength,seqdecode,FMX2Multiply,GKIFree,FreeAlignment,__vfprintf_chk,MasterTraceFromMap,IsInt,MSAAppendGR,MSAFileClose,Plan7SetDescription,SantaCruzCorrection,DArgMin,Plan7RenormalizeExits,PrintASCIIHistogram,ResizePlan7Matrix,SampleGamma,__ctype_b_loc,GSIAddFileToIndex,SSIFreeIndex,SSICreateIndex,sre_hton32,sqd_regerror,PrintIscore,P7Maxmodelmaker,SSIGetOffsetByName,floorf,RandomAlignment,sprintf,MSAExpand,FileConcat,GSCWeights,SSISetFilePosition,Plan7SetNullModel,WriteAscHMM,strrchr,PrintNewHampshireTree,Lawless422,StrMarkov1,DSet,Plan7NakedConfig,FilterAlignment,__strpbrk_c3,SSIErrorString,FSet,rkcomp,MSAGetGC,TraceSimpleBounds,EVDMaxLikelyFit,AllocPlan7Body,pow,P9ZeroHMM,CompareRefMultAlignments,FArgMin,strstr,Lawless416,seqndecode,SSIForceExternalSort,rewind,DisplayPlan7Matrix,strcat,seqncmp,MSAToSqinfo,P7WeeViterbiSize,fputs,SSIGetFilePosition,MakeAlignedString,strtol,SampleCountvector,SSIRecommendMode,TophitsMaxName,SSIAddPrimaryKeyToIndex,ShadowTrace,__libc_csu_init,ReverseIntStack,atoi,FreePlan7,EPSWriteSmallMSA,AlignmentHomogenousGapsym,DMin,LogSum,P7Logoddsify,AllocPlan7,StringChop,P7FreeTrace,DArgMax,Byteswap,Strdelete,FreeSequence,sre_toupper,GSIOpen,hmmcalibrate,DedigitizeSequence,EVDDistribution,DisplayPlan7Posteriors,__strcpy_chk,P7ViterbiTrace,DMX2Free,WriteSELEXOneBlock,sre_malloc,PrintPlan7Stats,P9FreeHMM,MSAFilePositionByIndex,GSIAddKeyToIndex,EVDrandom,MSAAddGS,strspn,sre_hton64,SampleAlignment,DetermineAlphabet,TraceCompare,P7OptimalAccuracy,XNU,memmove,strchr,sre_strtok,FNorm,WriteSELEX,P7Forward,GSISortIndex,DSum,fread,getenv,P9Renormalize,P7DefaultPrior,GSIGetRecord,sre_realloc,P9AllocHMM,WriteProfile,Plan7SetCtime,seqcmp,FAdd,ParsePAMFile,DMX2Alloc,P7TraceScore,__cyg_profile_func_exit,FArgMax,ToRNA,FSum,DCopy,sqd_regsub,SSIAddFileToIndex,MSAFileWrite,DMax,LogNorm,hit_comparison,GSIGetOffset,Cluster,WritePhylip,P7ReverseTrace,FreeIntStack,AlphabetType2String,__stack_chk_fail,SeqfileRewind,DChoose,MSAVerifyParse,DNorm,ReadStockholm,Plan7FSConfig,strcmp,tolower,__fread_chk,ExtremeValueP2,MSAGetSeqSS,MSAMingap,ReadA2M,UnfitHistogram,Free3DArray,StrRegionalShuffle,rkseq,FChoose,SAMizeAlignment,strcpy,GetRankedHit,strtok,FreeTophits,Plan7ESTViterbi,Plan7ComlogAppend,sre_srandom,StrReverse,EVDCensoredFit,FullSortTophits,SSIOpen,P7CountSymbol,P7Traces2Alignment,FreeHistogram,Logp_cvec,__ctype_tolower_loc,calloc,SqdClean,Plan7LSConfig,feof,WriteA2M,ExtremeValueE,SeqfileOpen,AlignmentShuffle,DLog,fclose,remove,FLogSum,PrintXMGRHistogram,Score2Prob,strncpy,GuessAlignmentSeqtype,P7Viterbi,AllocTophits,P7ViterbiAlignAlignment,EVDBasicFit,sre_tolower,SSIGetOffsetByNumber,SSISetFileForSubseq,MSAAppendGC,Plan7ESTConfig,HMMFileClose,FileExists,HMMFileRead,Plan9toPlan7,P7SmallViterbi,MSAFilePositionByKey,AllocPlan7Shell,__sprintf_chk,DealignedLength,FLog,FMX2Alloc,DigitizeSequence,strcspn,SeqinfoCopy,ExtremeValueSetHistogram,SAMizeAlignmentByGapFrac,DigitizeAlignment,GKIInit,AllocHistogram,TraceScoreCorrection,fopen64,EmitSequence,MSAFromAINFO,P7EmitterPosterior,Plan7Renormalize,FreePlan7Matrix,PopIntStack,fwrite,P7ReadNullModel,CreateFancyAli,MSAFileOpen,MSAFileRewind,MakeIdentityMx,realloc,ComparePairAlignments,floor,FileAddSuffix,perror,MakeDealignedString,sre_ntoh32,BlosumWeights,FileSameDirectory,WriteStockholmOneBlock,fprintf,localtime,AllocPhylo,HMMFileOpen,ReadMultipleRseqs,P7Fastmodelmaker,sre_ntoh64,seqencode,strftime,SSIClose,WriteClustal,StrShuffle,sqrt,strtoul,GKIStatus,PrintXMGRRegressionLine,s2upper,sre_random,memcpy,String2SeqfileFormat,SetSeqinfoString,P7ViterbiSize,Gammln,Getopt,__strncat_chk,MergeTraceArrays,log,WritePairwiseAlignment,AlignmentBootstrap,MSAGetSeqSA,logf,ExtremeValueFitHistogram,GSIWriteIndex,strtod,P7PriorifyTransitionVecto,SSIGetSubseqOffset,Panic,sqd_regexec,time,main,PushIntStack,_init,fflush,sqrtf,specqsort,ToIUPAC,Statetype,SSIWriteIndex,P_PvecGivenDirichlet,sre_ntoh16,coded_revcomp'
HULL_FUNCS='abort@@GLIBC_2.2.5,atof@@GLIBC_2.2.5,atoi@@GLIBC_2.2.5,atol@@GLIBC_2.2.5,__cxa_atexit@@GLIBC_2.2.5,__cyg_profile_func_enter,__cyg_profile_func_exit,deregister_tm_clones,__do_global_dtors_aux,_fini,frame_dummy,free@@GLIBC_2.2.5,gettimeofday@@GLIBC_2.2.5,_GLOBAL__sub_I__Z15serialQuickHullPiP8_point2dIdEiii,_GLOBAL__sub_I__ZN7benchIO13stringToWordsEPcl,GOMP_parallel_end@@GOMP_1.0,GOMP_parallel_start@@GOMP_1.0,__gxx_personality_v0@@CXXABI_1.3,_init,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,main,malloc@@GLIBC_2.2.5,mallopt@@GLIBC_2.2.5,memcmp@@GLIBC_2.2.5,omp_get_num_threads@@OMP_1.0,omp_get_thread_num@@OMP_1.0,__pthread_key_create@@GLIBC_2.2.5,register_tm_clones,sprintf@@GLIBC_2.2.5,__stack_chk_fail@@GLIBC_2.4,_start,_Unwind_Resume@@GCC_3.0,serialQuickHull(int*, _point2d<double>*, int, int, int),__static_initialization_and_destruction_0(int, int),__static_initialization_and_destruction_0(int, int),hull(_point2d<double>*, int),hull(_point2d<double>*, int) [clone ._omp_fn.10],hull(_point2d<double>*, int) [clone ._omp_fn.8],hull(_point2d<double>*, int) [clone ._omp_fn.9],std::pair<int, int> split<int, aboveLine>(int*, int, aboveLine, aboveLine),triArea(_point2d<double>, _point2d<double>, _point2d<double>),timeHull(_point2d<double>*, int, int, char*),quickHull(int*, int*, _point2d<double>*, int, int, int, int),quickHull(int*, int*, _point2d<double>*, int, int, int, int) [clone ._omp_fn.0],quickHull(int*, int*, _point2d<double>*, int, int, int, int) [clone ._omp_fn.1],triangArea::triangArea(int*, _point2d<double>*, int, int),triangArea::triangArea(int*, _point2d<double>*, int, int),triangArea::operator()(int),commandLine::badArgument(),commandLine::getArgument(int),commandLine::getOptionValue(std::basic_string<char, std::char_traits<char>, std::allocator<char> >),commandLine::getOptionIntValue(std::basic_string<char, std::char_traits<char>, std::allocator<char> >, int),commandLine::commandLine(int, char**, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),commandLine::commandLine(int, char**, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),commandLine::~commandLine(),commandLine::~commandLine(),minMaxIndex::minMaxIndex(_point2d<double>*),minMaxIndex::minMaxIndex(_point2d<double>*),minMaxIndex::operator()(std::pair<int, int>, std::pair<int, int>),_seq<_point2d<double> >::_seq(_point2d<double>*, long),_seq<_point2d<double> >::_seq(_point2d<double>*, long),_seq<char>::del(),_seq<char>::_seq(char*, long),_seq<char>::_seq(char*, long),_seq<int>::del(),_seq<int>::_seq(int*, long),_seq<int>::_seq(),_seq<int>::_seq(int*, long),_seq<int>::_seq(),_seq<long>::_seq(long*, long),_seq<long>::_seq(long*, long),timer::next(),timer::start(),timer::getTime(),timer::reportT(double),timer::timer(),timer::timer(),utils::identityF<long>::operator()(long const&),utils::logUp(unsigned int),utils::logUp(unsigned int),utils::myAssert(int, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),utils::myAssert(int, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),utils::logUpLong(unsigned long),utils::logUpLong(unsigned long),void benchIO::parsePoints<_point2d<double> >(char**, _point2d<double>*, int),void benchIO::parsePoints<_point2d<double> >(char**, _point2d<double>*, int) [clone ._omp_fn.13],void benchIO::parsePoints<_point2d<double> >(char**, _point2d<double>*, int) [clone ._omp_fn.14],benchIO::xToStringLen(int),_seq<char> benchIO::arrayToString<int>(int*, long),_seq<char> benchIO::arrayToString<int>(int*, long) [clone ._omp_fn.7],_seq<char> benchIO::arrayToString<int>(int*, long) [clone ._omp_fn.8],_seq<char> benchIO::arrayToString<int>(int*, long) [clone ._omp_fn.9],benchIO::stringToWords(char*, long),benchIO::stringToWords(char*, long) [clone ._omp_fn.0],benchIO::stringToWords(char*, long) [clone ._omp_fn.1],benchIO::stringToWords(char*, long) [clone ._omp_fn.2],int benchIO::writeArrayToFile<int>(std::basic_string<char, std::char_traits<char>, std::allocator<char> >, int*, long, char*),benchIO::writeStringToFile(char*, long, char*),_seq<_point2d<double> > benchIO::readPointsFromFile<_point2d<double> >(char*),benchIO::readStringFromFile(char*),void benchIO::writeArrayToStream<int>(std::basic_ofstream<char, std::char_traits<char> >&, int*, long),int benchIO::writeIntArrayToFile<int>(int*, long, char*),benchIO::readTrianglesFromFileNodeEle(char*),benchIO::words::words(char*, long, char**, long),benchIO::words::words(char*, long, char**, long),benchIO::isSpace(char),benchIO::notZero::operator()(char),benchIO::xToString(char*, int),_vect2d<double>::cross(_vect2d<double>),_vect2d<double>::_vect2d(double, double),_vect2d<double>::_vect2d(double, double),makePair::operator()(int),_point2d<double>::_point2d(double, double),_point2d<double>::_point2d(double*),_point2d<double>::_point2d(double, double),_point2d<double>::_point2d(double*),_point2d<double>::operator-(_point2d<double>),_seq<char> sequence::packSerial<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>),_seq<int> sequence::packSerial<int, int, sequence::getA<int, int> >(int*, bool*, int, int, sequence::getA<int, int>),_seq<long> sequence::packSerial<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>),int sequence::scanSerial<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool),long sequence::scanSerial<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool),int sequence::reduceSerial<int, int, utils::addF<int>, sequence::getA<int, int> >(int, int, utils::addF<int>, sequence::getA<int, int>),long sequence::reduceSerial<long, long, utils::addF<long>, sequence::getA<long, long> >(long, long, utils::addF<long>, sequence::getA<long, long>),std::pair<int, int> sequence::reduceSerial<std::pair<int, int>, int, minMaxIndex, makePair>(int, int, minMaxIndex, makePair),std::pair<int, int> sequence::reduceSerial<std::pair<int, int>, int, minMaxIndex, sequence::getA<std::pair<int, int>, int> >(int, int, minMaxIndex, sequence::getA<std::pair<int, int>, int>),int sequence::maxIndexSerial<double, int, std::greater<double>, triangArea>(int, int, std::greater<double>, triangArea),int sequence::sumFlagsSerial<int>(bool*, int),long sequence::sumFlagsSerial<long>(bool*, long),sequence::getA<char, long>::getA(char*),sequence::getA<char, long>::getA(char*),sequence::getA<char, long>::operator()(long),sequence::getA<int, int>::getA(int*),sequence::getA<int, int>::getA(int*),sequence::getA<int, int>::operator()(int),sequence::getA<long, long>::getA(long*),sequence::getA<long, long>::getA(long*),sequence::getA<long, long>::operator()(long),sequence::getA<std::pair<int, int>, int>::getA(std::pair<int, int>*),sequence::getA<std::pair<int, int>, int>::getA(std::pair<int, int>*),sequence::getA<std::pair<int, int>, int>::operator()(int),long sequence::pack<char, long>(char*, char*, bool*, long),_seq<char> sequence::pack<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>),_seq<char> sequence::pack<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>) [clone ._omp_fn.11],_seq<char> sequence::pack<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>) [clone ._omp_fn.12],int sequence::pack<int, int>(int*, int*, bool*, int),_seq<int> sequence::pack<int, int, sequence::getA<int, int> >(int*, bool*, int, int, sequence::getA<int, int>),_seq<int> sequence::pack<int, int, sequence::getA<int, int> >(int*, bool*, int, int, sequence::getA<int, int>) [clone ._omp_fn.4],_seq<int> sequence::pack<int, int, sequence::getA<int, int> >(int*, bool*, int, int, sequence::getA<int, int>) [clone ._omp_fn.5],_seq<long> sequence::pack<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>),_seq<long> sequence::pack<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>) [clone ._omp_fn.3],_seq<long> sequence::pack<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>) [clone ._omp_fn.4],int sequence::scan<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool),int sequence::scan<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool) [clone ._omp_fn.6],int sequence::scan<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool) [clone ._omp_fn.7],long sequence::scan<long, long, utils::addF<long> >(long*, long*, long, utils::addF<long>, long),long sequence::scan<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool),long sequence::scan<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool) [clone ._omp_fn.5],long sequence::scan<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool) [clone ._omp_fn.6],long sequence::filter<char, long, benchIO::notZero>(char*, char*, long, benchIO::notZero),long sequence::filter<char, long, benchIO::notZero>(char*, char*, long, benchIO::notZero) [clone ._omp_fn.10],int sequence::filter<int, int, aboveLine>(int*, int*, int, aboveLine),int sequence::filter<int, int, aboveLine>(int*, int*, int, aboveLine) [clone ._omp_fn.3],std::pair<int, int> sequence::reduce<std::pair<int, int>, int, minMaxIndex, makePair>(int, int, minMaxIndex, makePair),std::pair<int, int> sequence::reduce<std::pair<int, int>, int, minMaxIndex, makePair>(int, int, minMaxIndex, makePair) [clone ._omp_fn.11],std::pair<int, int> sequence::reduce<std::pair<int, int>, int, minMaxIndex, sequence::getA<std::pair<int, int>, int> >(int, int, minMaxIndex, sequence::getA<std::pair<int, int>, int>),std::pair<int, int> sequence::reduce<std::pair<int, int>, int, minMaxIndex, sequence::getA<std::pair<int, int>, int> >(int, int, minMaxIndex, sequence::getA<std::pair<int, int>, int>) [clone ._omp_fn.12],int sequence::maxIndex<double, int, std::greater<double>, triangArea>(int, int, std::greater<double>, triangArea),int sequence::maxIndex<double, int, std::greater<double>, triangArea>(int, int, std::greater<double>, triangArea) [clone ._omp_fn.2],int sequence::plusScan<int, int>(int*, int*, int),long sequence::plusScan<long, long>(long*, long*, long),_seq<long> sequence::packIndex<long>(bool*, long),aboveLine::aboveLine(_point2d<double>*, int, int),aboveLine::aboveLine(_point2d<double>*, int, int),aboveLine::operator()(int),triangles<_point2d<double> >::triangles(),triangles<_point2d<double> >::triangles(),utils::addF<int>::operator()(int const&, int const&) const,utils::addF<long>::operator()(long const&, long const&) const,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::data() const@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::size() const@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::c_str() const@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::compare(char const*) const@@GLIBCXX_3.4,std::fpos<__mbstate_t>::operator long() const,std::greater<double>::operator()(double const&, double const&) const,std::allocator<char>::allocator()@@GLIBCXX_3.4,std::allocator<char>::~allocator()@@GLIBCXX_3.4,std::basic_istream<char, std::char_traits<char> >::read(char*, long)@@GLIBCXX_3.4,std::basic_istream<char, std::char_traits<char> >::seekg(long, std::_Ios_Seekdir)@@GLIBCXX_3.4,std::basic_istream<char, std::char_traits<char> >::tellg()@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::write(char const*, long)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(double)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(std::basic_ostream<char, std::char_traits<char> >& (*)(std::basic_ostream<char, std::char_traits<char> >&))@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::append(char const*)@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::basic_string(char const*, std::allocator<char> const&)@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::basic_string(std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::~basic_string()@@GLIBCXX_3.4,std::char_traits<char>::compare(char const*, char const*, unsigned long),std::basic_ifstream<char, std::char_traits<char> >::close()@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::is_open()@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::basic_ifstream(char const*, std::_Ios_Openmode)@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::~basic_ifstream()@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::close()@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::is_open()@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::basic_ofstream(char const*, std::_Ios_Openmode)@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::~basic_ofstream()@@GLIBCXX_3.4,std::pair<int, int>::pair(int const&, int const&),std::pair<int, int>::pair(int const&, int const&),std::ios_base::Init::Init()@@GLIBCXX_3.4,std::ios_base::Init::~Init()@@GLIBCXX_3.4,std::setprecision(int),int const& std::min<int>(int const&, int const&),long const& std::min<long>(long const&, long const&),std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@@GLIBCXX_3.4,__gnu_cxx::__enable_if<std::__is_char<char>::__value, bool>::__type std::operator==<char>(std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&),bool std::operator==<char, std::char_traits<char>, std::allocator<char> >(char const*, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&),std::basic_ostream<char, std::char_traits<char> >& std::operator<< <char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, std::_Setprecision)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >& std::operator<< <char, std::char_traits<char>, std::allocator<char> >(std::basic_ostream<char, std::char_traits<char> >&, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@@GLIBCXX_3.4,bool std::operator!=<char, std::char_traits<char>, std::allocator<char> >(char const*, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&),std::operator|(std::_Ios_Openmode, std::_Ios_Openmode)'
LBM_FUNCS='atoi@@GLIBC_2.2.5,__cyg_profile_func_enter,__cyg_profile_func_exit,deregister_tm_clones,__do_global_dtors_aux,exit@@GLIBC_2.2.5,fclose@@GLIBC_2.2.5,fgetc@@GLIBC_2.2.5,_fini,fopen@@GLIBC_2.2.5,fprintf@@GLIBC_2.2.5,frame_dummy,fread@@GLIBC_2.2.5,free@@GLIBC_2.2.5,fwrite@@GLIBC_2.2.5,_init,__isoc99_fscanf@@GLIBC_2.7,LBM_allocateGrid,LBM_compareVelocityField,LBM_freeGrid,LBM_handleInOutFlow,LBM_initializeGrid,LBM_initializeSpecialCellsForChannel,LBM_initializeSpecialCellsForLDC,LBM_loadObstacleFile,LBM_performStreamCollide,LBM_showGridStatistics,LBM_storeVelocityField,LBM_swapGrids,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,loadValue,main,MAIN_finalize,MAIN_initialize,MAIN_parseCommandLine,MAIN_printInfo,malloc@@GLIBC_2.2.5,printf@@GLIBC_2.2.5,puts@@GLIBC_2.2.5,register_tm_clones,sqrt@@GLIBC_2.2.5,__stack_chk_fail@@GLIBC_2.4,_start,stat,__stat,storeValue,__xstat@@GLIBC_2.2.5'
NBODY_FUNCS='abort@@GLIBC_2.2.5,atof@@GLIBC_2.2.5,atoi@@GLIBC_2.2.5,atol@@GLIBC_2.2.5,__cxa_atexit@@GLIBC_2.2.5,__cyg_profile_func_enter,__cyg_profile_func_exit,deregister_tm_clones,__do_global_dtors_aux,_fini,floor@@GLIBC_2.2.5,frame_dummy,free@@GLIBC_2.2.5,gettimeofday@@GLIBC_2.2.5,_GLOBAL__sub_I__Z5checkPP8particlei,_GLOBAL__sub_I__ZN7benchIO13stringToWordsEPcl,GOMP_parallel_end@@GOMP_1.0,GOMP_parallel_start@@GOMP_1.0,__gxx_personality_v0@@CXXABI_1.3,_init,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,main,malloc@@GLIBC_2.2.5,mallopt@@GLIBC_2.2.5,memcmp@@GLIBC_2.2.5,omp_get_num_threads@@OMP_1.0,omp_get_thread_num@@OMP_1.0,__powidf2@@GCC_4.0.0,__pthread_key_create@@GLIBC_2.2.5,register_tm_clones,sprintf@@GLIBC_2.2.5,sqrt@@GLIBC_2.2.5,__stack_chk_fail@@GLIBC_2.4,_start,_Unwind_Resume@@GCC_3.0,xToStringLen(_point3d<double>),__static_initialization_and_destruction_0(int, int),__static_initialization_and_destruction_0(int, int),check(particle**, int),nbody(particle**, int),stepBH(particle**, int, double),stepBH(particle**, int, double) [clone ._omp_fn.0],forceTo(particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>*, double),timeNBody(_point3d<double>*, int, int, char*),timeNBody(_point3d<double>*, int, int, char*) [clone ._omp_fn.7],timeNBody(_point3d<double>*, int, int, char*) [clone ._omp_fn.8],xToString(char*, _point3d<double>),operator delete[](void*)@@GLIBCXX_3.4,operator delete(void*)@@GLIBCXX_3.4,blockTrans<particle*, int>::trans(int, int),blockTrans<particle*, int>::transR(int, int, int, int, int, int),blockTrans<particle*, int>::blockTrans(particle**, particle**, int*, int*, int*),blockTrans<particle*, int>::blockTrans(particle**, particle**, int*, int*, int*),centerMass::force(_point3d<double>, double),centerMass::centerMass(_point3d<double>),centerMass::centerMass(_point3d<double>),centerMass::operator+=(particle*),centerMass::operator+=(centerMass),commandLine::badArgument(),commandLine::getArgument(int),commandLine::getOptionValue(std::basic_string<char, std::char_traits<char>, std::allocator<char> >),commandLine::getOptionIntValue(std::basic_string<char, std::char_traits<char>, std::allocator<char> >, int),commandLine::commandLine(int, char**, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),commandLine::commandLine(int, char**, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),commandLine::~commandLine(),commandLine::~commandLine(),_seq<_point3d<double> >::_seq(_point3d<double>*, long),_seq<_point3d<double> >::_seq(_point3d<double>*, long),_seq<char>::del(),_seq<char>::_seq(char*, long),_seq<char>::_seq(char*, long),_seq<long>::_seq(long*, long),_seq<long>::_seq(long*, long),timer::reportNext(std::basic_string<char, std::char_traits<char>, std::allocator<char> >),timer::reportNext(),timer::reportTime(double),timer::next(),timer::start(),timer::getTime(),timer::reportT(double),timer::timer(),timer::timer(),utils::hash(unsigned int),utils::identityF<long>::operator()(long const&),utils::logUp(unsigned int),utils::logUp(unsigned int),int utils::log2Up<int>(int),utils::myAssert(int, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),utils::myAssert(int, std::basic_string<char, std::char_traits<char>, std::allocator<char> >),utils::logUpLong(unsigned long),utils::logUpLong(unsigned long),void benchIO::parsePoints<_point3d<double> >(char**, _point3d<double>*, int),void benchIO::parsePoints<_point3d<double> >(char**, _point3d<double>*, int) [clone ._omp_fn.15],void benchIO::parsePoints<_point3d<double> >(char**, _point3d<double>*, int) [clone ._omp_fn.16],benchIO::xToStringLen(double),_seq<char> benchIO::arrayToString<_point3d<double> >(_point3d<double>*, long),_seq<char> benchIO::arrayToString<_point3d<double> >(_point3d<double>*, long) [clone ._omp_fn.10],_seq<char> benchIO::arrayToString<_point3d<double> >(_point3d<double>*, long) [clone ._omp_fn.11],_seq<char> benchIO::arrayToString<_point3d<double> >(_point3d<double>*, long) [clone ._omp_fn.9],benchIO::stringToWords(char*, long),benchIO::stringToWords(char*, long) [clone ._omp_fn.0],benchIO::stringToWords(char*, long) [clone ._omp_fn.1],benchIO::stringToWords(char*, long) [clone ._omp_fn.2],int benchIO::writeArrayToFile<_point3d<double> >(std::basic_string<char, std::char_traits<char>, std::allocator<char> >, _point3d<double>*, long, char*),int benchIO::writePointsToFile<_point3d<double> >(_point3d<double>*, int, char*),benchIO::writeStringToFile(char*, long, char*),_seq<_point3d<double> > benchIO::readPointsFromFile<_point3d<double> >(char*),benchIO::readStringFromFile(char*),void benchIO::writeArrayToStream<_point3d<double> >(std::basic_ofstream<char, std::char_traits<char> >&, _point3d<double>*, long),benchIO::readTrianglesFromFileNodeEle(char*),benchIO::words::words(char*, long, char**, long),benchIO::words::words(char*, long, char**, long),benchIO::isSpace(char),benchIO::notZero::operator()(char),benchIO::xToString(char*, double),long intSort::iSortSpace<particle*, int>(int),void intSort::radixBlock<particle*, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>, int>(particle**, particle**, unsigned char*, int*, int*, int, int, int, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>),void intSort::radixStepSerial<particle*, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>, int>(particle**, particle**, unsigned char*, int*, int, int, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>),void intSort::radixLoopTopDown<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, particle**, unsigned char*, int (*) [256], int, int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild),void intSort::radixLoopTopDown<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, particle**, unsigned char*, int (*) [256], int, int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild) [clone ._omp_fn.11],void intSort::radixLoopBottomUp<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, particle**, unsigned char*, int (*) [256], int, int, int, bool, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild),intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>::eBits(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild),intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>::eBits(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild),intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>::operator()(particle*),void intSort::iSort<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, int*, int, int, bool, char*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild),void intSort::iSort<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, int*, int, int, bool, char*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild) [clone ._omp_fn.5],void intSort::iSort<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, int*, int, int, bool, char*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild) [clone ._omp_fn.6],void intSort::iSort<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, int*, int, int, bool, char*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild) [clone ._omp_fn.7],void intSort::iSort<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, int*, int, int, bool, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild),void intSort::iSort<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild, int>(particle**, int*, int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild),void intSort::radixStep<particle*, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>, int>(particle**, particle**, unsigned char*, int (*) [256], int, int, int, bool, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>),void intSort::radixStep<particle*, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>, int>(particle**, particle**, unsigned char*, int (*) [256], int, int, int, bool, intSort::eBits<particle*, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild>) [clone ._omp_fn.8],_vect3d<double>::dot(_vect3d<double>),_vect3d<double>::Length(),_vect3d<double>::maxDim(),_vect3d<double>::_vect3d(double, double, double),_vect3d<double>::_vect3d(),_vect3d<double>::_vect3d(double, double, double),_vect3d<double>::_vect3d(),_vect3d<double>::operator/(double),_vect3d<double>::operator*(double),_vect3d<double>::operator+(_vect3d<double>),particle::particle(_point3d<double>, double),particle::particle(_point3d<double>, double),_point2d<double>::_point2d(double, double),_point2d<double>::_point2d(double, double),_point3d<double>::offsetPoint(int, double),_point3d<double>::quadrant(_point3d<double>),_point3d<double>::dimension(),_point3d<double>::maxCoords(_point3d<double>),_point3d<double>::minCoords(_point3d<double>),_point3d<double>::_point3d(_vect3d<double>),_point3d<double>::_point3d(double, double, double),_point3d<double>::_point3d(double*),_point3d<double>::_point3d(),_point3d<double>::_point3d(_vect3d<double>),_point3d<double>::_point3d(double, double, double),_point3d<double>::_point3d(double*),_point3d<double>::_point3d(),_point3d<double>::operator-(_point3d<double>),_point3d<double>::operator+(_vect3d<double>),_seq<char> sequence::packSerial<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>),_seq<long> sequence::packSerial<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>),int sequence::scanSerial<int, int, utils::addF<int> >(int*, int*, int, utils::addF<int>, int),int sequence::scanSerial<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool),int sequence::scanSerial<int, int, utils::minF<int>, sequence::getA<int, int> >(int*, int, int, utils::minF<int>, sequence::getA<int, int>, int, bool, bool),long sequence::scanSerial<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool),_point3d<double> sequence::reduceSerial<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt, sequence::getA<_point3d<double>, int> >(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt, sequence::getA<_point3d<double>, int>),_point3d<double> sequence::reduceSerial<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt, sequence::getA<_point3d<double>, int> >(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt, sequence::getA<_point3d<double>, int>),int sequence::reduceSerial<int, int, utils::addF<int>, sequence::getA<int, int> >(int, int, utils::addF<int>, sequence::getA<int, int>),int sequence::reduceSerial<int, int, utils::minF<int>, sequence::getA<int, int> >(int, int, utils::minF<int>, sequence::getA<int, int>),long sequence::reduceSerial<long, long, utils::addF<long>, sequence::getA<long, long> >(long, long, utils::addF<long>, sequence::getA<long, long>),long sequence::sumFlagsSerial<long>(bool*, long),sequence::getA<_point3d<double>, int>::getA(_point3d<double>*),sequence::getA<_point3d<double>, int>::getA(_point3d<double>*),sequence::getA<_point3d<double>, int>::operator()(int),sequence::getA<char, long>::getA(char*),sequence::getA<char, long>::getA(char*),sequence::getA<char, long>::operator()(long),sequence::getA<int, int>::getA(int*),sequence::getA<int, int>::getA(int*),sequence::getA<int, int>::operator()(int),sequence::getA<long, long>::getA(long*),sequence::getA<long, long>::getA(long*),sequence::getA<long, long>::operator()(long),long sequence::pack<char, long>(char*, char*, bool*, long),_seq<char> sequence::pack<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>),_seq<char> sequence::pack<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>) [clone ._omp_fn.13],_seq<char> sequence::pack<char, long, sequence::getA<char, long> >(char*, bool*, long, long, sequence::getA<char, long>) [clone ._omp_fn.14],_seq<long> sequence::pack<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>),_seq<long> sequence::pack<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>) [clone ._omp_fn.3],_seq<long> sequence::pack<long, long, utils::identityF<long> >(long*, bool*, long, long, utils::identityF<long>) [clone ._omp_fn.4],int sequence::scan<int, int, utils::addF<int> >(int*, int*, int, utils::addF<int>, int),int sequence::scan<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool),int sequence::scan<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool) [clone ._omp_fn.10],int sequence::scan<int, int, utils::addF<int>, sequence::getA<int, int> >(int*, int, int, utils::addF<int>, sequence::getA<int, int>, int, bool, bool) [clone ._omp_fn.9],int sequence::scan<int, int, utils::minF<int>, sequence::getA<int, int> >(int*, int, int, utils::minF<int>, sequence::getA<int, int>, int, bool, bool),int sequence::scan<int, int, utils::minF<int>, sequence::getA<int, int> >(int*, int, int, utils::minF<int>, sequence::getA<int, int>, int, bool, bool) [clone ._omp_fn.12],int sequence::scan<int, int, utils::minF<int>, sequence::getA<int, int> >(int*, int, int, utils::minF<int>, sequence::getA<int, int>, int, bool, bool) [clone ._omp_fn.13],long sequence::scan<long, long, utils::addF<long> >(long*, long*, long, utils::addF<long>, long),long sequence::scan<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool),long sequence::scan<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool) [clone ._omp_fn.5],long sequence::scan<long, long, utils::addF<long>, sequence::getA<long, long> >(long*, long, long, utils::addF<long>, sequence::getA<long, long>, long, bool, bool) [clone ._omp_fn.6],long sequence::filter<char, long, benchIO::notZero>(char*, char*, long, benchIO::notZero),long sequence::filter<char, long, benchIO::notZero>(char*, char*, long, benchIO::notZero) [clone ._omp_fn.12],_point3d<double> sequence::reduce<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt>(_point3d<double>*, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt),_point3d<double> sequence::reduce<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt, sequence::getA<_point3d<double>, int> >(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt, sequence::getA<_point3d<double>, int>),_point3d<double> sequence::reduce<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt, sequence::getA<_point3d<double>, int> >(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt, sequence::getA<_point3d<double>, int>) [clone ._omp_fn.4],_point3d<double> sequence::reduce<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt>(_point3d<double>*, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt),_point3d<double> sequence::reduce<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt, sequence::getA<_point3d<double>, int> >(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt, sequence::getA<_point3d<double>, int>),_point3d<double> sequence::reduce<_point3d<double>, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt, sequence::getA<_point3d<double>, int> >(int, int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt, sequence::getA<_point3d<double>, int>) [clone ._omp_fn.3],long sequence::plusScan<long, long>(long*, long*, long),_seq<long> sequence::packIndex<long>(bool*, long),int sequence::scanIBack<int, int, utils::minF<int> >(int*, int*, int, utils::minF<int>, int),void gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::applyIndex<gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::flatten_FA>(int, gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::flatten_FA),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::flatten_FA::flatten_FA(particle**),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::flatten_FA::flatten_FA(particle**),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::flatten_FA::operator()(particle*, int),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findQuadrant(particle*),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::del(),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::gTree(particle**, int),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::gTree(particle**, int) [clone ._omp_fn.1],gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::gTree(particle**, int) [clone ._omp_fn.2],gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::maxpt::operator()(_point3d<double>, _point3d<double>),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::minpt::operator()(_point3d<double>, _point3d<double>),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::IsLeaf(),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::flatten(),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::newTree(particle**, int, _point3d<double>, double),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild::findChild(gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>*),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild::findChild(gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>*),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::findChild::operator()(particle*),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::gTreeNode(particle**, int, _point3d<double>, double),gTreeNode<_point3d<double>, _vect3d<double>, particle, centerMass>::gTreeNode(particle**, int, _point3d<double>, double),Transform<3, 9>::precompute(),Transform<3, 9>::Transform(),Transform<3, 9>::Transform(),transpose<int, int>::trans(int, int),transpose<int, int>::transR(int, int, int, int, int, int),transpose<int, int>::transpose(int*, int*),transpose<int, int>::transpose(int*, int*),triangles<_point2d<double> >::triangles(),triangles<_point2d<double> >::triangles(),operator new[](unsigned long)@@GLIBCXX_3.4,utils::addF<int>::operator()(int const&, int const&) const,utils::addF<long>::operator()(long const&, long const&) const,utils::minF<int>::operator()(int const&, int const&) const,Transform<3, 9>::factorial(double) const,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::data() const@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::size() const@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::c_str() const@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::compare(char const*) const@@GLIBCXX_3.4,std::fpos<__mbstate_t>::operator long() const,std::allocator<char>::allocator()@@GLIBCXX_3.4,std::allocator<char>::~allocator()@@GLIBCXX_3.4,std::basic_istream<char, std::char_traits<char> >::read(char*, long)@@GLIBCXX_3.4,std::basic_istream<char, std::char_traits<char> >::seekg(long, std::_Ios_Seekdir)@@GLIBCXX_3.4,std::basic_istream<char, std::char_traits<char> >::tellg()@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::write(char const*, long)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(double)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(int)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >::operator<<(std::basic_ostream<char, std::char_traits<char> >& (*)(std::basic_ostream<char, std::char_traits<char> >&))@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::append(char const*)@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::basic_string(char const*, std::allocator<char> const&)@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::basic_string(std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)@@GLIBCXX_3.4,std::basic_string<char, std::char_traits<char>, std::allocator<char> >::~basic_string()@@GLIBCXX_3.4,std::char_traits<char>::compare(char const*, char const*, unsigned long),std::basic_ifstream<char, std::char_traits<char> >::close()@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::is_open()@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::basic_ifstream(char const*, std::_Ios_Openmode)@@GLIBCXX_3.4,std::basic_ifstream<char, std::char_traits<char> >::~basic_ifstream()@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::close()@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::is_open()@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::basic_ofstream(char const*, std::_Ios_Openmode)@@GLIBCXX_3.4,std::basic_ofstream<char, std::char_traits<char> >::~basic_ofstream()@@GLIBCXX_3.4,std::complex<double>::complex(double, double),std::complex<double>::complex(double, double),std::ios_base::Init::Init()@@GLIBCXX_3.4,std::ios_base::Init::~Init()@@GLIBCXX_3.4,operator new(unsigned long)@@GLIBCXX_3.4,operator new(unsigned long, void*),std::setprecision(int),double const& std::max<double>(double const&, double const&),int const& std::max<int>(int const&, int const&),double const& std::min<double>(double const&, double const&),int const& std::min<int>(int const&, int const&),long const& std::min<long>(long const&, long const&),std::pow(double, int),std::basic_ostream<char, std::char_traits<char> >& std::endl<char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&)@@GLIBCXX_3.4,__gnu_cxx::__enable_if<std::__is_char<char>::__value, bool>::__type std::operator==<char>(std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&),bool std::operator==<char, std::char_traits<char>, std::allocator<char> >(char const*, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&),std::basic_ostream<char, std::char_traits<char> >& std::operator<< <char, std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, std::_Setprecision)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >& std::operator<< <char, std::char_traits<char>, std::allocator<char> >(std::basic_ostream<char, std::char_traits<char> >&, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&)@@GLIBCXX_3.4,std::basic_ostream<char, std::char_traits<char> >& std::operator<< <std::char_traits<char> >(std::basic_ostream<char, std::char_traits<char> >&, char const*)@@GLIBCXX_3.4,bool std::operator!=<char, std::char_traits<char>, std::allocator<char> >(char const*, std::basic_string<char, std::char_traits<char>, std::allocator<char> > const&),std::operator|(std::_Ios_Openmode, std::_Ios_Openmode)'
PERL_FUNCS='abort@@GLIBC_2.2.5,allocate_context,amagic_cmp,amagic_cmp_locale,amagic_i_ncmp,amagic_ncmp,argspec_compile,array_call,atan2@@GLIBC_2.2.5,atoi@@GLIBC_2.2.5,base64_16,boot_attrs,boot_Cwd,boot_Data__Dumper,boot_Devel__Peek,boot_Digest__MD5,boot_DynaLoader,boot_HTML__Parser,boot_IO,boot_MIME__Base64,boot_Opcode,boot_Storable,boot_Sys__Hostname,boot_Time__HiRes,bsd_realpath,calloc@@GLIBC_2.2.5,ceil@@GLIBC_2.2.5,chdir@@GLIBC_2.2.5,check_handler,chmod@@GLIBC_2.2.5,clean_context,clean_retrieve_context,clean_store_context,clearerr@@GLIBC_2.2.5,clear_re,closedir@@GLIBC_2.2.5,close@@GLIBC_2.2.5,cmp_desc,cmpindir,cmpindir_desc,constant,constant_11,const_sv_xsub,cos@@GLIBC_2.2.5,__ctype_b_loc@@GLIBC_2.3,__cyg_profile_func_enter,__cyg_profile_func_exit,dclone,DD_dump,DeadCode,decode_entities,deregister_tm_clones,do_clean_all,do_clean_named_objs,do_clean_objs,__do_global_dtors_aux,do_retrieve,do_store,dup2,dup@@GLIBC_2.2.5,dynprep,__errno_location@@GLIBC_2.2.5,esc_q,esc_q_utf8,execl@@GLIBC_2.2.5,execv@@GLIBC_2.2.5,execvp@@GLIBC_2.2.5,_exit@@GLIBC_2.2.5,exit@@GLIBC_2.2.5,exp@@GLIBC_2.2.5,F0convert,fclose@@GLIBC_2.2.5,fcntl@@GLIBC_2.2.5,fdopen@@GLIBC_2.2.5,feof@@GLIBC_2.2.5,ferror@@GLIBC_2.2.5,fflush@@GLIBC_2.2.5,fgetc@@GLIBC_2.2.5,fileno@@GLIBC_2.2.5,fill_mstats,_fini,floor@@GLIBC_2.2.5,flush_pending_text,fmod@@GLIBC_2.2.5,fopen@@GLIBC_2.2.5,fork@@GLIBC_2.2.5,fprintf@@GLIBC_2.2.5,frame_dummy,fread@@GLIBC_2.2.5,free_context,free@@GLIBC_2.2.5,free_pstate,freopen@@GLIBC_2.2.5,frexp@@GLIBC_2.2.5,fseek@@GLIBC_2.2.5,fstat,__fstat,ftell@@GLIBC_2.2.5,ftruncate@@GLIBC_2.2.5,fwrite@@GLIBC_2.2.5,__fxstat@@GLIBC_2.2.5,gcvt@@GLIBC_2.2.5,getcwd@@GLIBC_2.2.5,getegid@@GLIBC_2.2.5,getenv@@GLIBC_2.2.5,geteuid@@GLIBC_2.2.5,get_flag,getgid@@GLIBC_2.2.5,get_md5_ctx,get_op_bitspec,getpid@@GLIBC_2.2.5,get_pstate_hv,get_pstate_iv,gettimeofday@@GLIBC_2.2.5,getuid@@GLIBC_2.2.5,gmtime@@GLIBC_2.2.5,grow_gap,has_hibit,hex_16,hrt_usleep,htovl,htovs,_init,init_perinterp,init_retrieve_context,init_store_context,io_blocking,ioctl@@GLIBC_2.2.5,isatty@@GLIBC_2.2.5,__isnan@@GLIBC_2.2.5,is_retrieving,is_storing,kill@@GLIBC_2.2.5,known_class,last_op_in_netorder,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,link@@GLIBC_2.2.5,localtime@@GLIBC_2.2.5,log@@GLIBC_2.2.5,lseek@@GLIBC_2.2.5,magic_check,magic_free_pstate,magic_write,main,make_mortal_sv,malloc@@GLIBC_2.2.5,marked_section_update,mbuf2sv,MD5Final,MD5Init,MD5Transform,MD5Update,memchr@@GLIBC_2.2.5,memcmp@@GLIBC_2.2.5,memcpy@@GLIBC_2.14,memmove@@GLIBC_2.2.5,memset@@GLIBC_2.2.5,mkdir@@GLIBC_2.2.5,modf@@GLIBC_2.2.5,modify_SV_attributes,mretrieve,mstats2hash,mstats_fillhash,mstore,myNVtime,myU2time,needs_quote,net_mstore,net_pstore,new_opset,not_here,num_q,old_retrieve_array,old_retrieve_hash,opendir@@GLIBC_2.2.5,open@@GLIBC_2.2.5,opmask_add,opmask_addlocal,op_names_init,parse,parse_buf,parse_comment,parse_decl,parse_end,parse_marked_section,parse_process,parse_start,perl_alloc,Perl_allocmy,Perl_amagic_call,Perl_append_elem,Perl_append_list,Perl_apply,Perl_apply_attrs_string,Perl_atfork_lock,Perl_atfork_unlock,Perl_av_clear,Perl_av_delete,Perl_av_exists,Perl_av_extend,Perl_av_fake,Perl_av_fetch,Perl_av_fill,Perl_avhv_delete_ent,Perl_avhv_exists_ent,Perl_avhv_fetch_ent,Perl_avhv_iternext,Perl_avhv_iterval,Perl_avhv_keys,Perl_avhv_store_ent,Perl_av_len,Perl_av_make,Perl_av_pop,Perl_av_push,Perl_av_reify,Perl_av_shift,Perl_av_store,Perl_av_undef,Perl_av_unshift,Perl_bind_match,Perl_block_end,Perl_block_gimme,Perl_block_start,Perl_boot_core_PerlIO,Perl_boot_core_UNIVERSAL,Perl_boot_core_xsutils,Perl_bytes_from_utf8,Perl_bytes_to_utf8,Perl_call_argv,Perl_call_atexit,Perl_call_list,Perl_call_method,Perl_calloc,Perl_call_pv,Perl_call_sv,Perl_cando,Perl_cast_i32,Perl_cast_iv,Perl_cast_ulong,Perl_cast_uv,Perl_ck_anoncode,Perl_ck_bitop,Perl_ck_concat,Perl_ck_defined,Perl_ck_delete,Perl_ck_die,Perl_ck_eof,Perl_ck_eval,Perl_ck_exec,Perl_ck_exists,Perl_ck_exit,Perl_ck_ftst,Perl_ck_fun,Perl_ck_glob,Perl_ck_grep,Perl_ck_index,Perl_ck_join,Perl_ck_lengthconst,Perl_ck_lfun,Perl_ck_listiob,Perl_ck_match,Perl_ck_method,Perl_ck_null,Perl_ck_open,Perl_ck_repeat,Perl_ck_require,Perl_ck_return,Perl_ck_rfun,Perl_ck_rvconst,Perl_ck_sassign,Perl_ck_select,Perl_ck_shift,Perl_ck_sort,Perl_ck_spair,Perl_ck_split,Perl_ck_subr,Perl_ck_substr,Perl_ck_svconst,Perl_ck_trunc,perl_construct,Perl_convert,Perl_croak,Perl_csighandler,Perl_custom_op_desc,Perl_custom_op_name,Perl_cv_ckproto,Perl_cv_clone,Perl_cv_const_sv,Perl_cv_undef,Perl_cx_dump,Perl_cxinc,Perl_deb,Perl_debop,Perl_debprofdump,Perl_debstack,Perl_deb_stack_all,Perl_debstackptrs,Perl_delimcpy,Perl_deprecate,Perl_deprecate_old,Perl_despatch_signals,perl_destruct,Perl_die,Perl_die_where,Perl_do_aexec,Perl_do_aexec5,Perl_do_binmode,Perl_do_chomp,Perl_do_chop,Perl_do_close,Perl_do_dump_pad,Perl_do_eof,Perl_do_exec,Perl_do_exec3,Perl_do_execfree,Perl_dofile,Perl_do_gv_dump,Perl_do_gvgv_dump,Perl_do_hv_dump,Perl_doing_taint,Perl_do_join,Perl_do_kv,Perl_do_magic_dump,Perl_do_op_dump,Perl_do_open,Perl_do_open9,Perl_do_openn,Perl_do_pmop_dump,Perl_do_print,Perl_do_readline,Perl_do_seek,Perl_do_sprintf,Perl_do_sv_dump,Perl_do_sysseek,Perl_do_tell,Perl_do_trans,Perl_dounwind,Perl_do_vecget,Perl_do_vecset,Perl_do_vop,Perl_dowantarray,Perl_dump_all,Perl_dump_eval,Perl_dump_form,Perl_dump_indent,Perl_dump_packsubs,Perl_dump_sub,Perl_dump_vindent,Perl_eval_pv,Perl_eval_sv,Perl_fbm_compile,Perl_fbm_instr,Perl_filter_add,Perl_filter_del,Perl_filter_read,Perl_find_runcv,Perl_find_script,Perl_fold_constants,Perl_force_list,Perl_form,Perl_fprintf_nocontext,perl_free,Perl_free_tied_hv_pool,Perl_free_tmps,Perl_gen_constant_list,Perl_get_av,Perl_get_context,Perl_get_cv,Perl_getcwd_sv,Perl_getenv_len,Perl_get_hash_seed,Perl_get_hv,Perl_get_no_modify,Perl_get_opargs,Perl_get_op_descs,Perl_get_op_names,Perl_get_ppaddr,Perl_get_sv,Perl_get_vtbl,Perl_gp_free,Perl_gp_ref,Perl_grok_bin,Perl_grok_hex,Perl_grok_number,Perl_grok_numeric_radix,Perl_grok_oct,Perl_Gv_AMupdate,Perl_gv_autoload4,Perl_gv_AVadd,Perl_gv_check,Perl_gv_dump,Perl_gv_efullname,Perl_gv_efullname3,Perl_gv_efullname4,Perl_gv_fetchfile,Perl_gv_fetchmeth,Perl_gv_fetchmeth_autoload,Perl_gv_fetchmethod,Perl_gv_fetchmethod_autoload,Perl_gv_fetchpv,Perl_gv_fullname,Perl_gv_fullname3,Perl_gv_fullname4,Perl_gv_handler,Perl_gv_HVadd,Perl_gv_init,Perl_gv_IOadd,Perl_gv_stashpv,Perl_gv_stashpvn,Perl_gv_stashsv,Perl_huge,Perl_hv_clear,Perl_hv_clear_placeholders,Perl_hv_delayfree_ent,Perl_hv_delete,Perl_hv_delete_ent,Perl_hv_exists,Perl_hv_exists_ent,Perl_hv_fetch,Perl_hv_fetch_ent,Perl_hv_free_ent,Perl_hv_iterinit,Perl_hv_iterkey,Perl_hv_iterkeysv,Perl_hv_iternext,Perl_hv_iternext_flags,Perl_hv_iternextsv,Perl_hv_iterval,Perl_hv_ksplit,Perl_hv_magic,Perl_hv_scalar,Perl_hv_store,Perl_hv_store_ent,Perl_hv_store_flags,Perl_hv_undef,Perl_ibcmp,Perl_ibcmp_locale,Perl_ibcmp_utf8,Perl_ingroup,Perl_init_argv_symbols,Perl_init_debugger,Perl_init_i18nl10n,Perl_init_i18nl14n,Perl_init_stacks,Perl_init_tm,Perl_instr,Perl_intro_my,Perl_invert,PerlIO_allocate,PerlIO_apply_layera,PerlIO_apply_layers,PerlIO_arg_fetch,PerlIOBase_binmode,PerlIOBase_clearerr,PerlIOBase_close,PerlIOBase_dup,PerlIOBase_eof,PerlIOBase_error,PerlIOBase_fileno,PerlIOBase_flush_linebuf,PerlIOBase_noop_fail,PerlIOBase_noop_ok,PerlIOBase_popped,PerlIOBase_pushed,PerlIOBase_read,PerlIOBase_setlinebuf,PerlIOBase_unread,PerlIO_binmode,PerlIOBuf_bufsiz,PerlIOBuf_close,PerlIOBuf_dup,PerlIOBuf_fill,PerlIOBuf_flush,PerlIOBuf_get_base,PerlIOBuf_get_cnt,PerlIOBuf_get_ptr,PerlIOBuf_open,PerlIOBuf_popped,PerlIOBuf_pushed,PerlIOBuf_read,PerlIOBuf_seek,PerlIOBuf_set_ptrcnt,PerlIOBuf_tell,PerlIOBuf_unread,PerlIOBuf_write,PerlIO_canset_cnt,PerlIO_cleantable,PerlIO_cleanup,PerlIO_clone,PerlIO_clone_list,Perl_io_close,PerlIO__close,PerlIO_context_layers,PerlIOCrlf_binmode,PerlIOCrlf_flush,PerlIOCrlf_get_cnt,PerlIOCrlf_pushed,PerlIOCrlf_set_ptrcnt,PerlIOCrlf_unread,PerlIOCrlf_write,PerlIO_debug,PerlIO_default_buffer,PerlIO_default_layer,PerlIO_default_layers,PerlIO_define_layer,PerlIO_destruct,PerlIO_exportFILE,PerlIO_fast_gets,PerlIO_fdopen,PerlIO_fdupopen,PerlIO_findFILE,PerlIO_find_layer,PerlIO_getc,PerlIO_get_layers,PerlIO_getname,PerlIO_getpos,PerlIO_has_base,PerlIO_has_cntptr,PerlIO_importFILE,PerlIO_init,PerlIO_intmode2str,PerlIO_isutf8,PerlIO_layer_fetch,PerlIO_layer_from_ref,PerlIO_list_alloc,PerlIO_list_free,PerlIO_list_push,PerlIO_modestr,PerlIO_open,PerlIO_openn,PerlIO_parse_layers,PerlIOPending_close,PerlIOPending_fill,PerlIOPending_flush,PerlIOPending_pushed,PerlIOPending_read,PerlIOPending_seek,PerlIOPending_set_ptrcnt,PerlIO_pop,PerlIOPop_pushed,PerlIO_printf,PerlIO_push,PerlIO_putc,PerlIO_puts,PerlIORaw_open,PerlIORaw_pushed,PerlIO_releaseFILE,PerlIO_reopen,PerlIO_resolve_layers,PerlIO_rewind,PerlIO_setpos,PerlIO_sprintf,PerlIOStdio_clearerr,PerlIOStdio_close,PerlIOStdio_dup,PerlIOStdio_eof,PerlIOStdio_error,PerlIOStdio_fileno,PerlIOStdio_fill,PerlIOStdio_flush,PerlIOStdio_get_base,PerlIOStdio_get_bufsiz,PerlIOStdio_get_cnt,PerlIOStdio_get_ptr,PerlIOStdio_invalidate_fileno,PerlIOStdio_mode,PerlIOStdio_open,PerlIOStdio_pushed,PerlIOStdio_read,PerlIOStdio_seek,PerlIOStdio_setlinebuf,PerlIOStdio_set_ptrcnt,PerlIOStdio_tell,PerlIOStdio_unread,PerlIOStdio_write,PerlIO_stdoutf,PerlIO_stdstreams,PerlIO_sv_dup,PerlIO_tab_sv,PerlIO_tmpfile,PerlIO_ungetc,PerlIOUnix_close,PerlIOUnix_dup,PerlIOUnix_fileno,PerlIOUnix_oflags,PerlIOUnix_open,PerlIOUnix_pushed,PerlIOUnix_read,PerlIOUnix_refcnt_dec,PerlIOUnix_refcnt_inc,PerlIOUnix_seek,PerlIOUnix_setfd,PerlIOUnix_tell,PerlIOUnix_write,PerlIOUtf8_pushed,PerlIO_vprintf,PerlIO_vsprintf,Perl_is_gv_magical,Perl_is_lvalue_sub,Perl_is_uni_alnum,Perl_is_uni_alnumc,Perl_is_uni_alnumc_lc,Perl_is_uni_alnum_lc,Perl_is_uni_alpha,Perl_is_uni_alpha_lc,Perl_is_uni_ascii,Perl_is_uni_ascii_lc,Perl_is_uni_cntrl,Perl_is_uni_cntrl_lc,Perl_is_uni_digit,Perl_is_uni_digit_lc,Perl_is_uni_graph,Perl_is_uni_graph_lc,Perl_is_uni_idfirst,Perl_is_uni_idfirst_lc,Perl_is_uni_lower,Perl_is_uni_lower_lc,Perl_is_uni_print,Perl_is_uni_print_lc,Perl_is_uni_punct,Perl_is_uni_punct_lc,Perl_is_uni_space,Perl_is_uni_space_lc,Perl_is_uni_upper,Perl_is_uni_upper_lc,Perl_is_uni_xdigit,Perl_is_uni_xdigit_lc,Perl_is_utf8_alnum,Perl_is_utf8_alnumc,Perl_is_utf8_alpha,Perl_is_utf8_ascii,Perl_is_utf8_char,Perl_is_utf8_cntrl,Perl_is_utf8_digit,Perl_is_utf8_graph,Perl_is_utf8_idcont,Perl_is_utf8_idfirst,Perl_is_utf8_lower,Perl_is_utf8_mark,Perl_is_utf8_print,Perl_is_utf8_punct,Perl_is_utf8_space,Perl_is_utf8_string,Perl_is_utf8_string_loc,Perl_is_utf8_upper,Perl_is_utf8_xdigit,Perl_jmaybe,Perl_keyword,Perl_leave_scope,Perl_lex_end,Perl_lex_start,Perl_linklist,Perl_list,Perl_listkids,Perl_load_module,Perl_localize,Perl_looks_like_number,Perl_magic_clear_all_env,Perl_magic_clearenv,Perl_magic_clearpack,Perl_magic_clearsig,Perl_magic_dump,Perl_magic_existspack,Perl_magic_freeovrld,Perl_magic_freeregexp,Perl_magic_get,Perl_magic_getarylen,Perl_magic_getdefelem,Perl_magic_getglob,Perl_magic_getnkeys,Perl_magic_getpack,Perl_magic_getpos,Perl_magic_getsig,Perl_magic_getsubstr,Perl_magic_gettaint,Perl_magic_getuvar,Perl_magic_getvec,Perl_magic_killbackrefs,Perl_magic_len,Perl_magicname,Perl_magic_nextpack,Perl_magic_regdata_cnt,Perl_magic_regdatum_get,Perl_magic_regdatum_set,Perl_magic_scalarpack,Perl_magic_set,Perl_magic_set_all_env,Perl_magic_setamagic,Perl_magic_setarylen,Perl_magic_setbm,Perl_magic_setdbline,Perl_magic_setdefelem,Perl_magic_setenv,Perl_magic_setfm,Perl_magic_setglob,Perl_magic_setisa,Perl_magic_setmglob,Perl_magic_setnkeys,Perl_magic_setpack,Perl_magic_setpos,Perl_magic_setregexp,Perl_magic_setsig,Perl_magic_setsubstr,Perl_magic_settaint,Perl_magic_setutf8,Perl_magic_setuvar,Perl_magic_setvec,Perl_magic_sizepack,Perl_magic_wipepack,Perl_malloc,Perl_markstack_grow,Perl_mess,Perl_mfree,Perl_mg_clear,Perl_mg_copy,Perl_mg_find,Perl_mg_free,Perl_mg_get,Perl_mg_length,Perl_mg_magical,Perl_mg_set,Perl_mg_size,Perl_mini_mktime,Perl_mod,Perl_mode_from_discipline,Perl_moreswitches,Perl_my,Perl_my_atof,Perl_my_atof2,Perl_my_attrs,Perl_my_exit,Perl_my_failure_exit,Perl_my_fflush_all,Perl_my_fork,Perl_my_htonl,Perl_my_lstat,Perl_my_ntohl,Perl_my_pclose,Perl_my_poll,Perl_my_popen,Perl_my_popen_list,Perl_my_setenv,Perl_my_socketpair,Perl_my_stat,Perl_my_strftime,Perl_my_swabn,Perl_my_swap,Perl_my_unexec,Perl_newANONATTRSUB,Perl_newANONHASH,Perl_newANONLIST,Perl_newANONSUB,Perl_newASSIGNOP,Perl_newATTRSUB,Perl_newAV,Perl_newAVREF,Perl_newBINOP,Perl_new_collate,Perl_newCONDOP,Perl_newCONSTSUB,Perl_new_ctype,Perl_newCVREF,Perl_newFORM,Perl_newFOROP,Perl_newGVgen,Perl_newGVOP,Perl_newGVREF,Perl_newHV,Perl_newHVhv,Perl_newHVREF,Perl_newIO,Perl_newLISTOP,Perl_newLOGOP,Perl_newLOOPEX,Perl_newLOOPOP,Perl_newMYSUB,Perl_newNULLLIST,Perl_new_numeric,Perl_newOP,Perl_newPADOP,Perl_newPMOP,Perl_newPROG,Perl_newPVOP,Perl_newRANGE,Perl_newRV,Perl_newRV_noinc,Perl_newSLICEOP,Perl_new_stackinfo,Perl_newSTATEOP,Perl_newSUB,Perl_newSV,Perl_newSViv,Perl_newSVnv,Perl_newSVOP,Perl_newSVpv,Perl_newSVpvf,Perl_newSVpvn,Perl_newSVpvn_share,Perl_newSVREF,Perl_newSVrv,Perl_newSVsv,Perl_newSVuv,Perl_newUNOP,Perl_newWHILEOP,Perl_newXS,Perl_nextargv,Perl_ninstr,Perl_nothreadhook,Perl_oopsAV,Perl_oopsCV,Perl_oopsHV,Perl_op_clear,Perl_op_const_sv,Perl_op_dump,Perl_op_free,Perl_op_null,Perl_op_refcnt_lock,Perl_op_refcnt_unlock,Perl_package,Perl_pack_cat,Perl_packlist,Perl_pad_add_anon,Perl_pad_add_name,Perl_pad_alloc,Perl_pad_block_start,Perl_pad_check_dup,Perl_pad_findmy,Perl_pad_fixup_inner_anons,Perl_pad_free,Perl_pad_leavemy,Perl_pad_new,Perl_pad_push,Perl_pad_reset,Perl_pad_sv,Perl_pad_swipe,Perl_pad_tidy,Perl_pad_undef,perl_parse,Perl_parse_unicode_opts,Perl_peep,Perl_PerlIO_clearerr,Perl_PerlIO_close,Perl_PerlIO_eof,Perl_PerlIO_error,Perl_PerlIO_fileno,Perl_PerlIO_fill,Perl_PerlIO_flush,Perl_PerlIO_get_base,Perl_PerlIO_get_bufsiz,Perl_PerlIO_get_cnt,Perl_PerlIO_get_ptr,Perl_PerlIO_read,Perl_PerlIO_seek,Perl_PerlIO_set_cnt,Perl_PerlIO_setlinebuf,Perl_PerlIO_set_ptrcnt,Perl_PerlIO_stderr,Perl_PerlIO_stdin,Perl_PerlIO_stdout,Perl_PerlIO_tell,Perl_PerlIO_unread,Perl_PerlIO_write,Perl_pidgone,Perl_pmflag,Perl_pmop_dump,Perl_pmruntime,Perl_pmtrans,Perl_pop_return,Perl_pop_scope,Perl_pp_aassign,Perl_pp_abs,Perl_pp_accept,Perl_pp_add,Perl_pp_aelem,Perl_pp_aelemfast,Perl_pp_alarm,Perl_pp_and,Perl_pp_andassign,Perl_pp_anoncode,Perl_pp_anonhash,Perl_pp_anonlist,Perl_pp_aslice,Perl_pp_atan2,Perl_pp_av2arylen,Perl_pp_backtick,Perl_pp_bind,Perl_pp_binmode,Perl_pp_bit_and,Perl_pp_bit_or,Perl_pp_bit_xor,Perl_pp_bless,Perl_pp_caller,Perl_pp_chdir,Perl_pp_chmod,Perl_pp_chomp,Perl_pp_chop,Perl_pp_chown,Perl_pp_chr,Perl_pp_chroot,Perl_pp_close,Perl_pp_closedir,Perl_pp_complement,Perl_pp_concat,Perl_pp_cond_expr,Perl_pp_connect,Perl_pp_const,Perl_pp_cos,Perl_pp_crypt,Perl_pp_dbmclose,Perl_pp_dbmopen,Perl_pp_dbstate,Perl_pp_defined,Perl_pp_delete,Perl_pp_die,Perl_pp_divide,Perl_pp_dofile,Perl_pp_dump,Perl_pp_each,Perl_pp_egrent,Perl_pp_ehostent,Perl_pp_enetent,Perl_pp_enter,Perl_pp_entereval,Perl_pp_enteriter,Perl_pp_enterloop,Perl_pp_entersub,Perl_pp_entertry,Perl_pp_enterwrite,Perl_pp_eof,Perl_pp_eprotoent,Perl_pp_epwent,Perl_pp_eq,Perl_pp_eservent,Perl_pp_exec,Perl_pp_exists,Perl_pp_exit,Perl_pp_exp,Perl_pp_fcntl,Perl_pp_fileno,Perl_pp_flip,Perl_pp_flock,Perl_pp_flop,Perl_pp_fork,Perl_pp_formline,Perl_pp_ftatime,Perl_pp_ftbinary,Perl_pp_ftblk,Perl_pp_ftchr,Perl_pp_ftctime,Perl_pp_ftdir,Perl_pp_fteexec,Perl_pp_fteowned,Perl_pp_fteread,Perl_pp_ftewrite,Perl_pp_ftfile,Perl_pp_ftis,Perl_pp_ftlink,Perl_pp_ftmtime,Perl_pp_ftpipe,Perl_pp_ftrexec,Perl_pp_ftrowned,Perl_pp_ftrread,Perl_pp_ftrwrite,Perl_pp_ftsgid,Perl_pp_ftsize,Perl_pp_ftsock,Perl_pp_ftsuid,Perl_pp_ftsvtx,Perl_pp_fttext,Perl_pp_fttty,Perl_pp_ftzero,Perl_pp_ge,Perl_pp_gelem,Perl_pp_getc,Perl_pp_getlogin,Perl_pp_getpeername,Perl_pp_getpgrp,Perl_pp_getppid,Perl_pp_getpriority,Perl_pp_getsockname,Perl_pp_ggrent,Perl_pp_ggrgid,Perl_pp_ggrnam,Perl_pp_ghbyaddr,Perl_pp_ghbyname,Perl_pp_ghostent,Perl_pp_glob,Perl_pp_gmtime,Perl_pp_gnbyaddr,Perl_pp_gnbyname,Perl_pp_gnetent,Perl_pp_goto,Perl_pp_gpbyname,Perl_pp_gpbynumber,Perl_pp_gprotoent,Perl_pp_gpwent,Perl_pp_gpwnam,Perl_pp_gpwuid,Perl_pp_grepstart,Perl_pp_grepwhile,Perl_pp_gsbyname,Perl_pp_gsbyport,Perl_pp_gservent,Perl_pp_gsockopt,Perl_pp_gt,Perl_pp_gv,Perl_pp_gvsv,Perl_pp_helem,Perl_pp_hex,Perl_pp_hslice,Perl_pp_i_add,Perl_pp_i_divide,Perl_pp_i_eq,Perl_pp_i_ge,Perl_pp_i_gt,Perl_pp_i_le,Perl_pp_i_lt,Perl_pp_i_modulo,Perl_pp_i_modulo_0,Perl_pp_i_modulo_1,Perl_pp_i_multiply,Perl_pp_i_ncmp,Perl_pp_index,Perl_pp_i_ne,Perl_pp_i_negate,Perl_pp_int,Perl_pp_ioctl,Perl_pp_i_subtract,Perl_pp_iter,Perl_pp_join,Perl_pp_keys,Perl_pp_kill,Perl_pp_last,Perl_pp_lc,Perl_pp_lcfirst,Perl_pp_le,Perl_pp_leave,Perl_pp_leaveeval,Perl_pp_leaveloop,Perl_pp_leavesub,Perl_pp_leavesublv,Perl_pp_leavetry,Perl_pp_leavewrite,Perl_pp_left_shift,Perl_pp_length,Perl_pp_lineseq,Perl_pp_link,Perl_pp_list,Perl_pp_listen,Perl_pp_localtime,Perl_pp_lock,Perl_pp_log,Perl_pp_lslice,Perl_pp_lstat,Perl_pp_lt,Perl_pp_mapstart,Perl_pp_mapwhile,Perl_pp_match,Perl_pp_method,Perl_pp_method_named,Perl_pp_mkdir,Perl_pp_modulo,Perl_pp_msgctl,Perl_pp_msgget,Perl_pp_msgrcv,Perl_pp_msgsnd,Perl_pp_multiply,Perl_pp_ncmp,Perl_pp_ne,Perl_pp_negate,Perl_pp_next,Perl_pp_nextstate,Perl_pp_not,Perl_pp_null,Perl_pp_oct,Perl_pp_open,Perl_pp_open_dir,Perl_pp_or,Perl_pp_orassign,Perl_pp_ord,Perl_pp_pack,Perl_pp_padany,Perl_pp_padav,Perl_pp_padhv,Perl_pp_padsv,Perl_pp_pipe_op,Perl_pp_pop,Perl_pp_pos,Perl_pp_postdec,Perl_pp_postinc,Perl_pp_pow,Perl_pp_predec,Perl_pp_preinc,Perl_pp_print,Perl_pp_prototype,Perl_pp_prtf,Perl_pp_push,Perl_pp_pushmark,Perl_pp_pushre,Perl_pp_qr,Perl_pp_quotemeta,Perl_pp_rand,Perl_pp_range,Perl_pp_rcatline,Perl_pp_read,Perl_pp_readdir,Perl_pp_readline,Perl_pp_readlink,Perl_pp_recv,Perl_pp_redo,Perl_pp_ref,Perl_pp_refgen,Perl_pp_regcmaybe,Perl_pp_regcomp,Perl_pp_regcreset,Perl_pp_rename,Perl_pp_repeat,Perl_pp_require,Perl_pp_reset,Perl_pp_return,Perl_pp_reverse,Perl_pp_rewinddir,Perl_pp_right_shift,Perl_pp_rindex,Perl_pp_rmdir,Perl_pp_rv2av,Perl_pp_rv2cv,Perl_pp_rv2gv,Perl_pp_rv2hv,Perl_pp_rv2sv,Perl_pp_sassign,Perl_pp_scalar,Perl_pp_schomp,Perl_pp_schop,Perl_pp_scmp,Perl_pp_scope,Perl_pp_seek,Perl_pp_seekdir,Perl_pp_select,Perl_pp_semctl,Perl_pp_semget,Perl_pp_semop,Perl_pp_send,Perl_pp_seq,Perl_pp_setpgrp,Perl_pp_setpriority,Perl_pp_setstate,Perl_pp_sge,Perl_pp_sgrent,Perl_pp_sgt,Perl_pp_shift,Perl_pp_shmctl,Perl_pp_shmget,Perl_pp_shmread,Perl_pp_shmwrite,Perl_pp_shostent,Perl_pp_shutdown,Perl_pp_sin,Perl_pp_sle,Perl_pp_sleep,Perl_pp_slt,Perl_pp_sne,Perl_pp_snetent,Perl_pp_socket,Perl_pp_sockpair,Perl_pp_sort,Perl_pp_splice,Perl_pp_split,Perl_pp_sprintf,Perl_pp_sprotoent,Perl_pp_spwent,Perl_pp_sqrt,Perl_pp_srand,Perl_pp_srefgen,Perl_pp_sselect,Perl_pp_sservent,Perl_pp_ssockopt,Perl_pp_stat,Perl_pp_stringify,Perl_pp_stub,Perl_pp_study,Perl_pp_subst,Perl_pp_substcont,Perl_pp_substr,Perl_pp_subtract,Perl_pp_symlink,Perl_pp_syscall,Perl_pp_sysopen,Perl_pp_sysread,Perl_pp_sysseek,Perl_pp_system,Perl_pp_syswrite,Perl_pp_tell,Perl_pp_telldir,Perl_pp_threadsv,Perl_pp_tie,Perl_pp_tied,Perl_pp_time,Perl_pp_tms,Perl_pp_trans,Perl_pp_truncate,Perl_pp_uc,Perl_pp_ucfirst,Perl_pp_umask,Perl_pp_undef,Perl_pp_unlink,Perl_pp_unpack,Perl_pp_unshift,Perl_pp_unstack,Perl_pp_untie,Perl_pp_utime,Perl_pp_values,Perl_pp_vec,Perl_pp_wait,Perl_pp_waitpid,Perl_pp_wantarray,Perl_pp_warn,Perl_pp_xor,Perl_pregcomp,Perl_pregexec,Perl_pregfree,Perl_prepend_elem,Perl_printf_nocontext,Perl_push_return,Perl_push_scope,Perl_pv_display,Perl_pv_uni_display,Perl_qerror,Perl_raise_signal,Perl_realloc,Perl_ref,Perl_refkids,Perl_regclass_swash,Perl_regdump,Perl_regexec_flags,Perl_reginitcolors,Perl_regnext,Perl_regprop,Perl_re_intuit_start,Perl_re_intuit_string,Perl_repeatcpy,Perl_report_evil_fh,Perl_report_uninit,Perl_require_pv,Perl_rninstr,Perl_rsignal,Perl_rsignal_restore,Perl_rsignal_save,Perl_rsignal_state,perl_run,Perl_runops_debug,Perl_runops_standard,Perl_rxres_free,Perl_rxres_restore,Perl_rxres_save,Perl_safesyscalloc,Perl_safesysfree,Perl_safesysmalloc,Perl_safesysrealloc,Perl_same_dirent,Perl_save_aelem,Perl_save_alloc,Perl_save_aptr,Perl_save_ary,Perl_save_bool,Perl_save_clearsv,Perl_save_delete,Perl_save_destructor,Perl_save_destructor_x,Perl_save_freeop,Perl_save_freepv,Perl_save_freesv,Perl_save_generic_pvref,Perl_save_generic_svref,Perl_save_gp,Perl_save_hash,Perl_save_helem,Perl_save_hints,Perl_save_hptr,Perl_save_I16,Perl_save_I32,Perl_save_I8,Perl_save_int,Perl_save_item,Perl_save_iv,Perl_save_list,Perl_save_long,Perl_save_mortalizesv,Perl_save_nogv,Perl_save_op,Perl_save_padsv,Perl_save_pptr,Perl_savepv,Perl_savepvn,Perl_save_re_context,Perl_save_scalar,Perl_savesharedpv,Perl_save_shared_pvref,Perl_save_sptr,Perl_savestack_grow,Perl_savestack_grow_cnt,Perl_savesvpv,Perl_save_svref,Perl_save_threadsv,Perl_save_vptr,Perl_sawparens,Perl_scalar,Perl_scalarkids,Perl_scalarseq,Perl_scalarvoid,Perl_scan_bin,Perl_scan_hex,Perl_scan_num,Perl_scan_oct,Perl_scan_vstring,Perl_scope,Perl_screaminstr,Perl_seed,Perl_set_context,Perl_setdefout,Perl_setenv_getix,Perl_set_numeric_local,Perl_set_numeric_radix,Perl_set_numeric_standard,Perl_share_hek,Perl_sighandler,perlsio_binmode,Perl_sortsv,Perl_stack_grow,Perl_start_glob,Perl_start_subparse,Perl_str_to_version,Perl_sub_crush_depth,Perl_sv_2bool,Perl_sv_2cv,Perl_sv_2io,Perl_sv_2iv,Perl_sv_2mortal,Perl_sv_2nv,Perl_sv_2pv,Perl_sv_2pvbyte,Perl_sv_2pvbyte_nolen,Perl_sv_2pv_flags,Perl_sv_2pv_nolen,Perl_sv_2pvutf8,Perl_sv_2pvutf8_nolen,Perl_sv_2uv,Perl_sv_add_arena,Perl_sv_backoff,Perl_sv_bless,Perl_sv_cat_decode,Perl_sv_catpv,Perl_sv_catpvf,Perl_sv_catpvf_mg,Perl_sv_catpv_mg,Perl_sv_catpvn,Perl_sv_catpvn_flags,Perl_sv_catpvn_mg,Perl_sv_catsv,Perl_sv_catsv_flags,Perl_sv_catsv_mg,Perl_sv_chop,Perl_sv_clean_all,Perl_sv_clean_objs,Perl_sv_clear,Perl_sv_cmp,Perl_sv_cmp_locale,Perl_sv_compile_2op,Perl_sv_copypv,Perl_sv_dec,Perl_sv_derived_from,Perl_sv_dump,Perl_sv_eq,Perl_sv_force_normal,Perl_sv_force_normal_flags,Perl_sv_free,Perl_sv_free_arenas,Perl_sv_gets,Perl_sv_grow,Perl_sv_inc,Perl_sv_insert,Perl_sv_isa,Perl_sv_isobject,Perl_sv_iv,Perl_sv_len,Perl_sv_len_utf8,Perl_sv_magic,Perl_sv_magicext,Perl_sv_mortalcopy,Perl_sv_newmortal,Perl_sv_newref,Perl_sv_nolocking,Perl_sv_nosharing,Perl_sv_nounlocking,Perl_sv_nv,Perl_sv_peek,Perl_sv_pos_b2u,Perl_sv_pos_u2b,Perl_sv_pv,Perl_sv_pvbyte,Perl_sv_pvbyten,Perl_sv_pvbyten_force,Perl_sv_pvn,Perl_sv_pvn_force,Perl_sv_pvn_force_flags,Perl_sv_pvn_nomg,Perl_sv_pvutf8,Perl_sv_pvutf8n,Perl_sv_pvutf8n_force,Perl_sv_recode_to_utf8,Perl_sv_reftype,Perl_sv_replace,Perl_sv_report_used,Perl_sv_reset,Perl_sv_rvweaken,Perl_sv_setiv,Perl_sv_setiv_mg,Perl_sv_setnv,Perl_sv_setnv_mg,Perl_sv_setpv,Perl_sv_setpvf,Perl_sv_setpvf_mg,Perl_sv_setpviv,Perl_sv_setpviv_mg,Perl_sv_setpv_mg,Perl_sv_setpvn,Perl_sv_setpvn_mg,Perl_sv_setref_iv,Perl_sv_setref_nv,Perl_sv_setref_pv,Perl_sv_setref_pvn,Perl_sv_setref_uv,Perl_sv_setsv,Perl_sv_setsv_flags,Perl_sv_setsv_mg,Perl_sv_setuv,Perl_sv_setuv_mg,Perl_sv_taint,Perl_sv_tainted,Perl_sv_true,Perl_sv_uni_display,Perl_sv_unmagic,Perl_sv_unref,Perl_sv_unref_flags,Perl_sv_untaint,Perl_sv_upgrade,Perl_sv_usepvn,Perl_sv_usepvn_mg,Perl_sv_utf8_decode,Perl_sv_utf8_downgrade,Perl_sv_utf8_encode,Perl_sv_utf8_upgrade,Perl_sv_utf8_upgrade_flags,Perl_sv_uv,Perl_sv_vcatpvf,Perl_sv_vcatpvf_mg,Perl_sv_vcatpvfn,Perl_sv_vsetpvf,Perl_sv_vsetpvf_mg,Perl_sv_vsetpvfn,Perl_swash_fetch,Perl_swash_init,Perl_taint_env,Perl_taint_proper,Perl_tmps_grow,Perl_to_uni_fold,Perl_to_uni_lower,Perl_to_uni_lower_lc,Perl_to_uni_title,Perl_to_uni_title_lc,Perl_to_uni_upper,Perl_to_uni_upper_lc,Perl_to_utf8_case,Perl_to_utf8_fold,Perl_to_utf8_lower,Perl_to_utf8_title,Perl_to_utf8_upper,Perl_unpack_str,Perl_unpackstring,Perl_unshare_hek,Perl_unsharepvn,Perl_utf16_to_utf8,Perl_utf16_to_utf8_reversed,Perl_utf8_distance,Perl_utf8_hop,Perl_utf8_length,Perl_utf8n_to_uvchr,Perl_utf8n_to_uvuni,Perl_utf8_to_bytes,Perl_utf8_to_uvchr,Perl_utf8_to_uvuni,Perl_utilize,Perl_uvchr_to_utf8,Perl_uvchr_to_utf8_flags,Perl_uvuni_to_utf8,Perl_uvuni_to_utf8_flags,Perl_vcroak,Perl_vdeb,Perl_vdie,Perl_vform,Perl_vivify_defelem,Perl_vivify_ref,Perl_vload_module,Perl_vmess,Perl_vnewSVpvf,Perl_vwarn,Perl_vwarner,Perl_wait4pid,Perl_warn,Perl_warner,Perl_watch,Perl_whichsig,Perl_write_to_stderr,Perl_yyerror,Perl_yylex,Perl_yyparse,Perl_yywarn,pipe@@GLIBC_2.2.5,pkg_can,pkg_fetchmeth,pkg_hide,pkg_uncache,pow@@GLIBC_2.2.5,pretrieve,probably_utf8_chunk,pstore,putenv@@GLIBC_2.2.5,put_op_bitspec,qsort@@GLIBC_2.2.5,readdir@@GLIBC_2.2.5,read_e_script,read@@GLIBC_2.2.5,realloc@@GLIBC_2.2.5,register_tm_clones,report_event,reset_context,restore_magic,restore_pos,restore_rsfp,retrieve,retrieve_array,retrieve_blessed,retrieve_byte,retrieve_code,retrieve_double,retrieve_flag_hash,retrieve_hash,retrieve_hook,retrieve_idx_blessed,retrieve_integer,retrieve_lscalar,retrieve_lutf8str,retrieve_netint,retrieve_other,retrieve_overloaded,retrieve_ref,retrieve_scalar,retrieve_sv_no,retrieve_sv_undef,retrieve_sv_yes,retrieve_tied_array,retrieve_tied_hash,retrieve_tied_idx,retrieve_tied_key,retrieve_tied_scalar,retrieve_undef,retrieve_utf8str,rmdir@@GLIBC_2.2.5,_runops_debug,run_user_filter,S_add_data,S_ao,S_apply_attrs,S_apply_attrs_my,S_asIV,S_asUV,S_avhv_index,S_avhv_index_sv,S_bad_type,S_cache_re,scalar_call,S_call_body,S_call_list_body,S_checkcomma,S_checkposixcc,S_check_uni,S_cl_and,S_cl_anything,S_cl_init,S_cl_init_zero,S_cl_is_anything,S_cl_or,S_closest_cop,S_cop_free,S_cv_clone2,S_deb_curcv,S_debprof,S_deb_stack_n,S_del_he,S_del_xiv,S_del_xnv,S_del_xpv,S_del_xpvav,S_del_xpvbm,S_del_xpvcv,S_del_xpvhv,S_del_xpviv,S_del_xpvlv,S_del_xpvmg,S_del_xpvnv,S_del_xrv,S_depcom,S_div128,S_docatch,S_docatch_body,S_doencodes,S_doeval,S_dofindlabel,S_doform,S_do_maybe_phash,S_do_oddball,S_doopen_pm,S_doparseform,S_dopoptoeval,S_dopoptolabel,S_dopoptoloop,S_dopoptosub,S_dopoptosub_at,S_do_trans_complex,S_do_trans_complex_utf8,S_do_trans_count,S_do_trans_count_utf8,S_do_trans_simple,S_do_trans_simple_utf8,S_dup_attrlist,select@@GLIBC_2.2.5,S_emulate_eaccess,setgid@@GLIBC_2.2.5,set_opset_bits,setuid@@GLIBC_2.2.5,setvbuf@@GLIBC_2.2.5,S_expect_number,S_filter_gets,S_find_beginning,S_find_byclass,S_find_in_my_stash,S_forbid_setid,S_force_ident,S_force_next,S_force_version,S_force_word,S_get_db_sub,S_get_num,S_group_end,S_gv_ename,S_gv_init_sv,S_hfreeentries,S_hsplit,S_hv_delete_common,S_hv_fetch_common,S_hv_magic_check,S_hv_notallowed,siglongjmp@@GLIBC_2.2.5,signal@@GLIBC_2.2.5,__sigsetjmp@@GLIBC_2.2.5,sig_trap,S_incline,S_incl_perldb,S_incpush,S_incpush_if_exists,sin@@GLIBC_2.2.5,S_init_ids,S_init_interp,S_init_lexer,S_init_main_stash,S_init_perllib,S_init_postdump_symbols,S_init_predump_symbols,S_intuit_method,S_intuit_more,S_isa_lookup,S_is_an_int,S_is_handle_constructor,skip_until_gt,sleep@@GLIBC_2.2.5,S_list_assignment,S_lop,S_magic_methcall,S_magic_methpack,S_measure_struct,S_mergesortsv,S_mess_alloc,S_method_common,S_missingterm,S_modkids,S_more_he,S_more_sv,S_more_xiv,S_more_xnv,S_more_xpv,S_more_xpvav,S_more_xpvbm,S_more_xpvcv,S_more_xpvhv,S_more_xpviv,S_more_xpvlv,S_more_xpvmg,S_more_xpvnv,S_more_xrv,S_mul128,S_mulexp10,S_my_exit_jump,S_my_kid,S_new_constant,S_newDEFSVOP,S_new_he,S_new_logop,S_new_xiv,S_new_xnv,S_new_xpv,S_new_xpvav,S_new_xpvbm,S_new_xpvcv,S_new_xpvhv,S_new_xpviv,S_new_xpvlv,S_new_xpvmg,S_new_xpvnv,S_new_xrv,S_nextchar,S_next_symbol,S_no_bareword_allowed,S_no_fh_allowed,S_no_op,S_not_a_number,S_nuke_stacks,S_num_overflow,S_open_script,sortcv,sortcv_stacked,sortcv_xsub,S_pack_rec,S_pad_findlex,S_parse_body,S_path_is_absolute,spec_rand,spec_srand,S_pending_ident,sprintf@@GLIBC_2.2.5,sqrt@@GLIBC_2.2.5,S_qsortsv,S_qsortsvu,S_re_croak2,S_refto,S_reg,S_reganode,S_regatom,S_regbranch,S_regclass,S_regcppop,S_regcppush,S_regcp_set_to,S_regcurly,S_reghop,S_reghop3,S_reghopmaybe,S_reghopmaybe3,S_reginclass,S_reginsert,S_regmatch,S_reg_node,S_regoptail,S_regpiece,S_regpposixcc,S_regrepeat,S_regrepeat_hard,S_regtail,S_regtry,S_reguni,S_regwhite,S_require_errno,S_run_body,S_save_hek_flags,S_save_lines,S_save_magic,S_save_scalar_at,S_scalarboolean,S_scalar_mod_type,S_scan_commit,S_scan_const,S_scan_formline,S_scan_heredoc,S_scan_ident,S_scan_inputsymbol,S_scan_pat,S_scan_str,S_scan_subst,S_scan_trans,S_scan_word,S_set_caret_X,S_set_csh,S_share_hek_flags,S_simplify_sort,S_skipspace,S_sortsv_desc,S_study_chunk,S_sublex_done,S_sublex_push,S_sublex_start,S_sv_2iuv_non_preserve,S_sv_add_backref,S_sv_del_backref,S_sv_unglob,S_swallow_bom,__stack_chk_fail@@GLIBC_2.4,_start,stat,__stat,S_to_byte_substr,S_tokeq,S_too_few_arguments,S_too_many_arguments,store,store_array,store_blessed,store_code,store_hash,store_hook,store_other,store_ref,store_scalar,store_tied,store_tied_item,S_to_utf8_substr,strcat@@GLIBC_2.2.5,strchr@@GLIBC_2.2.5,strcmp@@GLIBC_2.2.5,strcpy@@GLIBC_2.2.5,strerror@@GLIBC_2.2.5,strlen@@GLIBC_2.2.5,strncmp@@GLIBC_2.2.5,strncpy@@GLIBC_2.2.5,strnEQx,strrchr@@GLIBC_2.2.5,strtol@@GLIBC_2.2.5,S_unpack_rec,S_unshare_hek_or_pvn,S_usage,S_utf8_mg_pos,S_utf8_mg_pos_init,S_validate_suid,S_vdie_common,S_vdie_croak_common,sv_i_ncmp,S_visit,sv_lower,sv_ncmp,sv_type,sv_x,time@@GLIBC_2.2.5,tmpfile@@GLIBC_2.2.5,tokens_grow,truncate@@GLIBC_2.2.5,u2s,uiv_2buf,ungetc@@GLIBC_2.2.5,unlink@@GLIBC_2.2.5,unwind_handler_stack,utf16rev_textfilter,utf16_textfilter,uvcompare,verify_opset,vsprintf@@GLIBC_2.2.5,vtohl,vtohs,waitpid@@GLIBC_2.2.5,write@@GLIBC_2.2.5,XS_attributes_bootstrap,XS_attributes__fetch_attrs,XS_attributes__guess_stash,XS_attributes__modify_attrs,XS_attributes_reftype,XS_attributes__warn_reserved,XS_attrs_get,XS_attrs_import,XS_Cwd_abs_path,XS_Cwd_fastcwd,XS_Data__Dumper_Dumpxs,XS_Devel__Peek_CvGV,XS_Devel__Peek_DeadCode,XS_Devel__Peek_Dump,XS_Devel__Peek_DumpArray,XS_Devel__Peek_DumpProg,XS_Devel__Peek_fill_mstats,XS_Devel__Peek_mstat,XS_Devel__Peek_mstats2hash,XS_Devel__Peek_mstats_fillhash,XS_Devel__Peek_runops_debug,XS_Devel__Peek_SvREFCNT,XS_Devel__Peek_SvREFCNT_dec,XS_Devel__Peek_SvREFCNT_inc,XS_Digest__MD5_add,XS_Digest__MD5_addfile,XS_Digest__MD5_clone,XS_Digest__MD5_DESTROY,XS_Digest__MD5_digest,XS_Digest__MD5_md5,XS_Digest__MD5_new,XS_DynaLoader_dl_error,xsfclose,xsfopen,xsfprintf,XS_HTML__Entities__decode_entities,XS_HTML__Entities_decode_entities,XS_HTML__Entities__probably_utf8_chunk,XS_HTML__Entities_UNICODE_SUPPORT,XS_HTML__Parser__alloc_pstate,XS_HTML__Parser_boolean_attribute_value,XS_HTML__Parser_eof,XS_HTML__Parser_handler,XS_HTML__Parser_ignore_tags,XS_HTML__Parser_parse,XS_HTML__Parser_strict_comment,xs_init,XS_Internals_hash_seed,XS_Internals_hv_clear_placehold,XS_Internals_HvREHASH,XS_Internals_rehash_seed,XS_Internals_SvREADONLY,XS_Internals_SvREFCNT,XS_IO__File_new_tmpfile,XS_IO__Handle_blocking,XS_IO__Handle_clearerr,XS_IO__Handle_error,XS_IO__Handle_flush,XS_IO__Handle_setbuf,XS_IO__Handle_setvbuf,XS_IO__Handle_sync,XS_IO__Handle_ungetc,XS_IO__Handle_untaint,XS_IO__Poll__poll,XS_IO__Seekable_getpos,XS_IO__Seekable_setpos,XS_IO__Socket_sockatmark,XS_MIME__Base64_decode_base64,XS_MIME__Base64_encode_base64,XS_MIME__QuotedPrint_decode_qp,XS_MIME__QuotedPrint_encode_qp,XS_Opcode_define_optag,XS_Opcode_empty_opset,XS_Opcode_full_opset,XS_Opcode_invert_opset,XS_Opcode_opcodes,XS_Opcode_opdesc,XS_Opcode_opmask,XS_Opcode_opmask_add,XS_Opcode_opset,XS_Opcode_opset_to_ops,XS_Opcode_permit_only,XS_Opcode__safe_call_sv,XS_Opcode__safe_pkg_prep,XS_Opcode_verify_opset,XS_PerlIO_get_layers,XS_PerlIO__Layer__find,XS_PerlIO__Layer__NoWarnings,XS_Regexp_DESTROY,XS_Storable__Cxt_DESTROY,XS_Storable_dclone,XS_Storable_init_perinterp,XS_Storable_is_retrieving,XS_Storable_is_storing,XS_Storable_last_op_in_netorder,XS_Storable_mretrieve,XS_Storable_mstore,XS_Storable_net_mstore,XS_Storable_net_pstore,XS_Storable_pretrieve,XS_Storable_pstore,XS_Sys__Hostname_ghname,__xstat@@GLIBC_2.2.5,XS_Time__HiRes_constant,XS_Time__HiRes_gettimeofday,XS_Time__HiRes_sleep,XS_Time__HiRes_time,XS_Time__HiRes_usleep,XS_UNIVERSAL_can,XS_UNIVERSAL_isa,XS_UNIVERSAL_VERSION,XS_utf8_decode,XS_utf8_downgrade,XS_utf8_encode,XS_utf8_is_utf8,XS_utf8_native_to_unicode,XS_utf8_unicode_to_native,XS_utf8_upgrade,XS_utf8_valid,yydestruct'
SJENG_FUNCS='add_capture,addHolding,add_move,allocate_time,alloc_ecache,alloc_hash,__assert_fail@@GLIBC_2.2.5,atoi@@GLIBC_2.2.5,atol@@GLIBC_2.2.5,BegForPartner,Bishop,bishop_mobility,black_saccers,calc_attackers,calloc@@GLIBC_2.2.5,CheckBadFlow,checkECache,check_legal,check_phase,check_piece_square,check_solution,choose_book_move,clear_dp_tt,clear_tt,comp_to_coord,comp_to_san,__ctype_b_loc@@GLIBC_2.3,__cyg_profile_func_enter,__cyg_profile_func_exit,deregister_tm_clones,develop_node,difftime@@GLIBC_2.2.5,display_board,__do_global_dtors_aux,DropaddHolding,DropremoveHolding,ErrorIt,eval,exit@@GLIBC_2.2.5,extended_in_check,fclose@@GLIBC_2.2.5,fgets@@GLIBC_2.2.5,f_in_check,findlowest,_fini,fopen@@GLIBC_2.2.5,fprintf@@GLIBC_2.2.5,fputc@@GLIBC_2.2.5,fputs@@GLIBC_2.2.5,frame_dummy,free_ecache,free@@GLIBC_2.2.5,free_hash,freenodes,fwrite@@GLIBC_2.2.5,gen,GreetPartner,HandlePartner,HandlePtell,hash_extract_pv,in_check,_init,init_book,init_game,initialize_eval,initialize_hash,initialize_zobrist,interrupt,_IO_getc@@GLIBC_2.2.5,is_attacked,is_draw,is_move,__isoc99_sscanf@@GLIBC_2.7,King,Knight,l_bishop_mobility,LearnStoreTT,__libc_csu_fini,__libc_csu_init,__libc_start_main@@GLIBC_2.2.5,l_king_mobility,l_knight_mobility,losers_eval,losers_pn_eval,l_pawn_mobility,l_rook_mobility,main,make,malloc@@GLIBC_2.2.5,memset@@GLIBC_2.2.5,nk_attacked,order_moves,Pawn,perft,perft_debug,pn2_eval,pn_eval,post_fail_thinking,post_fh_thinking,post_fl_thinking,post_stat_thinking,post_thinking,printf@@GLIBC_2.2.5,printHolding,print_move,ProbeTT,ProcessHoldings,proofnumbercheck,proofnumberscan,proofnumbersearch,push_king,push_king_castle,push_knighT,push_pawn,push_pawn_simple,push_slidE,putchar@@GLIBC_2.2.5,PutPiece,puts@@GLIBC_2.2.5,QProbeTT,qsearch,QStoreTT,Queen,randomMT,rdelay,rdifftime,read_rcfile,register_tm_clones,reloadMT,removeHolding,remove_one,reset_board,reset_ecache,ResetHandValue,reset_piece_square,rinput,Rook,rook_mobility,rtime,run_autotest,run_epd_testsuite,s_bishop_mobility,search,search_root,see,seedMT,select_most_proving,setbuf@@GLIBC_2.2.5,set_proof_and_disproof_numbers,setup_attackers,setup_epd_line,signal@@GLIBC_2.2.5,s_king_mobility,s_knight_mobility,s_pawn_mobility,sprintf@@GLIBC_2.2.5,s_rook_mobility,__stack_chk_fail@@GLIBC_2.4,_start,start_up,std_eval,std_pn_eval,storeECache,StoreTT,strcat@@GLIBC_2.2.5,strchr@@GLIBC_2.2.5,strcmp@@GLIBC_2.2.5,strcpy@@GLIBC_2.2.5,stringize_pv,strncmp@@GLIBC_2.2.5,strncpy@@GLIBC_2.2.5,strstr@@GLIBC_2.2.5,suicide_eval,suicide_mid_eval,suicide_pn_eval,SwitchColor,SwitchPromoted,text_to_piece,think,toggle_bool,tolower@@GLIBC_2.2.5,tree,tree_debug,try_drop,unmake,update_ancestors,verify_coord,white_saccers,Xfree,Xmalloc'
#This script collects groundtruth data for
#specific functions

#Heavy hitter information (from Swarm)
#HULL: 
# _ZN7_vect2dIdE5crossES0_ Count 54951512 Avg time (cycles) : 126131434
# _Z7triArea8_point2dIdES0_ Count 54951514 Avg time (cycles) : 9382
# _ZN7_vect2dIdEC2Edd Count 109903028 Avg time (cycles) : 237
# _ZN8_point2dIdEmiES0_ Count 109903028 Avg time (cycles) : 2098
# _ZN7benchIO7isSpaceEc Count 369998027 Avg time (cycles) : 166
#_vect2d<double>::cross(_vect2d<double>)
#triArea(_point2d<double>, _point2d<double>)
#_vect2d<double>::_vect2d(double, double
#_point2d<double>::operator-(_point2d<double>)
#benchIO::isSpace(char)
HULL_INSTR_FUNCS='cross triArea _vect2d _point2d<double>::operator-(_point2d<double>) isSpace'

#BLACKSCHOLES:
# main Count 1 Avg time (cycles) : 47286391274
# _Z9bs_threadPv Count 8 Avg time (cycles) : 39360429908
# _Z19BlkSchlsEqEuroNoDivff Count 100000000 Avg time (cycles) : 2450
# _Z4CNDFf Count 200000000 Avg time (cycles) : 327
BLACKSCHOLES_INSTR_FUNCS='main bs_thread BlkSchlsEqEuroNoDiv CNDF' 

#BZIP
# mainGtU Count 328060278 Avg time (cycles) : 85
# bsW Count 79379955 Avg time (cycles) : 71
# mainSimpleSort Count 9986077 Avg time (cycles) : 6643
# add_pair_to_block Count 7247208 Avg time (cycles) : 96
# mmed3 Count 6966489 Avg time (cycles) : 71
BZIP_INSTR_FUNCS='mainGtU bsW mainSimpleSort add_pair_to_block mmed3'

#FLUID
# _ZN4Vec3mIERKS_ Count 268966388 Avg time (cycles) : 142
# _ZN4Vec3pLERKS_ Count 563014876 Avg time (cycles) : 155
# _ZNK4Vec3mlEf Count 1419720564 Avg time (cycles) : 1127
# _ZNK4Vec311GetLengthSqEv Count 3442875319 Avg time (cycles) : 159
# _ZNK4Vec3miERKS_ Count 3711841709 Avg time (cycles) : 1101
# _ZN4Vec3C2Efff Count 5206450909 Avg time (cycles) : 201

# THIS IS MESSED UP! 
FLUID_INSTR_FUNCS='Vec3::operator-=(Vec3 const&) Vec3::operator+=(Vec3 const&) Vec3::operator*(float) const\ GetLengthSq Vec3::operator-(Vec3\ const&) Vec3::Vec3(float,\ float,\ float)' 
# Are we interested in these functions!?? 

#H264
# PutPel_14 Count 11187200 Avg time (cycles) : 71
# SATD Count 32619882 Avg time (cycles) : 232
# UMVPelY_14 Count 51037136 Avg time (cycles) : 73
# UMVLine16Y_11 Count 66437600 Avg time (cycles) : 103
# FastLine16Y_11 Count 269932720 Avg time (cycles) : 64
# FastPelY_14 Count 543503824 Avg time (cycles) : 73
H264_INSTR_FUNCS='PutPel_14 SATD UMVPelY_14 UMVLine16Y_11 FastLine16Y_11 FastPelY_14'

#HMMER
# Gaussrandom Count 541551 Avg time (cycles) : 486
# sre_malloc Count 1000061 Avg time (cycles) : 247
# FChoose Count 277243628 Avg time (cycles) : 295
# SymbolIndex Count 277243628 Avg time (cycles) : 239
# toupper@GLIBC_2.2.5 Count 277243642 Avg time (cycles) : 65
# sre_random Count 277975300 Avg time (cycles) : 85
HMMER_INSTR_FUNCS='Gaussrandom sre_malloc FChoose SymbolIndex toupper sre_random'

#LBM 
# LBM_initializeGrid Count 2 Avg time (cycles) : 247744886
# LBM_initializeSpecialCell Count 2 Avg time (cycles) : 27789607
# LBM_loadObstacleFile Count 2 Avg time (cycles) : 60961630
# LBM_showGridStatistics Count 48 Avg time (cycles) : 124882607
# LBM_performStreamCollide Count 3000 Avg time (cycles) : 287415030
# LBM_swapGrids Count 3000 Avg time (cycles) : 377
LBM_INSTR_FUNCS='LBM_initializeGrid LBM_initializeSpecialCell LBM_loadObstacleFile LBM_showGridStatistics LBM_performStreamCollide LBM_swapGrids'

#NBODY
# _Z7forceToP8particleP9gTr Count 193245024 Avg time (cycles) : 24100
# _ZN7_vect3dIdEmlEd Count 207404980 Avg time (cycles) : 1894
# _ZN7_vect3dIdEplES0_ Count 233952072 Avg time (cycles) : 1898
# _ZN7_vect3dIdE3dotES0_ Count 554347948 Avg time (cycles) : 166
# _ZN8_point3dIdEmiES0_ Count 565347948 Avg time (cycles) : 1976
# _ZN7_vect3dIdEC1Eddd Count 1035252092 Avg time (cycles) : 210
NBODY_INSTR_FUNCS='forceTo _vect3d<double>::operator*(double) _vect3d<double>::operator+(_vect3d<double>) dot _point3d<double>::operator-(_point3d<double>) _vect3d<double>::_vect3d(double,\ double,\ double)'


#PERL
# Perl_sv_grow Count 278596803 Avg time (cycles) : 264
# Perl_sv_clear Count 282771095 Avg time (cycles) : 412
# Perl_sv_upgrade Count 282896404 Avg time (cycles) : 249
# S_regtry Count 289733260 Avg time (cycles) : 878
# Perl_sv_free Count 350561497 Avg time (cycles) : 497
# S_regmatch Count 522266916 Avg time (cycles) : 194
PERL_INSTR_FUNCS='Perl_sv_grow Perl_sv_clear Perl_sv_upgrade S_regtry Perl_sv_free S_regmatch'

#SJENG
# remove_one Count 8008480 Avg time (cycles) : 240
# make Count 8104038 Avg time (cycles) : 129
# unmake Count 8104038 Avg time (cycles) : 124
# add_move Count 10534138 Avg time (cycles) : 78
# push_king Count 13045696 Avg time (cycles) : 92
# Pawn Count 16566013 Avg time (cycles) : 92
# push_slidE Count 19136185 Avg time (cycles) : 196
SJENG_INSTR_FUNCS='remove_one make unmake add_move push_king Pawn push_slidE'

# Missing Functions in various tests 
#     h264 - SATD, UMVPelY
# hmmer - FCHoose, sre_malloc, sre_random, toupper
# lbm - LBM_initializeGrid, LBM_initializeSpecialCell
# perl - S_regtry, Perl_sv_free, S_regmatch
# sjeng - unmake, push_king

# Attempting to solve:
# LBM: 
#  removed duplication of LBM_initializeSpecialCell
#  removed 2 occurences of _init 
# 
# HMMER: 
#  removed malloc 
#  removed duplicated toUpper
#  rename FCHoose to FChoose
#  sre_random is still a mystery 

# SJENG: 
#  removed make 

# And the parallel benches have problems: 
# parallel beches
# doesn't work


# after set_profiled_func this holds the excludelist
EXCLUDED_LIST='' 

# removes the first occurence of the passed in string.
# i see that sometimes funcnames are repeated !! (hope this wont be the case
# for instrumented funcs 
function set_profiled_func {
   name=$1[@]
   a=("${!name}")
   
   echo "CONSTRUCTING EXCLUDED FUNCTION LIST" 
   echo "EXCLUDING="$2
   
   EXCLUDED_LIST=$(echo ${a[@]}  | sed "s@$2@@g")
   echo "EXCLUDED_LIST="$EXCLUDED_LIST
} 


#experiementing
# for apa in $SJENG_INSTR_FUNCS; do 
    
#     echo PROFILE_THIS=$apa
    
#     set_profiled_func SJENG_FUNCS $apa
#     echo $SJENG_FUNCS
#     echo EXCLUDED_LIST=$EXCLUDED_LIST
    
    
# done 


# set_profiled_func BLACKSCHOLES_FUNCS ", fflush"

# echo APAPAP
# echo $EXCLUDED_LIST 
# echo APAPAP
# echo $BLACKSCHOLES_FUNCS

#build ubiprof 
make lib   # CFLAGS='-DOVERHEAD_TIME_SERIES' 

BENCHROOT=benchmarks 
DATADIR=groundtruth

export CC=gcc

if [ ! -d $DATADIR ] ; then 
    mkdir $DATADIR
fi 

function build_it {
#          -finstrument-functions-exclude-function-list
    INSTR="-finstrument-functions-exclude-function-list="
    echo "EXPORTING EXCLUDE LIST"

    export EXCLUDED_FUNCTION_LIST=$INSTR"\"$EXCLUDED_LIST\""
    echo "$EXCLUDED_FUNCTION_LIST"
    BENCH=$1
    echo "BUILDING:"$BENCH

    (cd $BENCHROOT/$BENCH/ubiprof;EXCLUDED_FUNCTION_LIST=$INSTR"\"$EXCLUDED_LIST\"" make build)
}

: << 'EOF'
function build_it { 
#          -finstrument-functions-exclude-function-list
    INSTR="-finstrument-functions-exclude-function-list="
    echo "EXPORTING EXCLUDE LIST"
    
    export EXCLUDED_FUNCTION_LIST=$INSTR$EXCLUDED_LIST
    echo $EXCLUDED_FUNCTION_LIST
    BENCH=$1
    echo "BUILDING:"$BENCH
    
    (cd $BENCHROOT/$BENCH/ubiprof;EXCLUDED_FUNCTION_LIST=$INSTR$EXCLUDED_LIST make build)        
}

EOF

function build_it_regular { 
    BENCH=$1
    echo "BUILDING:"$BENCH
    
    (cd $BENCHROOT/$BENCH/ubiprof;make build)        
}

function run_it { 
    BENCH=$1
    ROUND=$2 
    FUNC=$3

    (cd $BENCHROOT/$BENCH/ubiprof; make run) 
    # will not work for the parallel benches!! 
    # that use a different directory structure (My bad idea) 
    #mv $BENCHROOT/$BENCH/instrumented/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
    mv $BENCHROOT/$BENCH/instrumented/prof.out $DATADIR/prof-$HOSTNAME-$BENCH-$ROUND-$FUNC.out
    mv $BENCHROOT/$BENCH/instrumented/statistics.out $DATADIR/stat-$HOSTNAME-$BENCH-$ROUND-$FUNC.out
}

function run_it_par { 
    BENCH=$1
    EXECDIR=$2
    ROUND=$3
    FUNC=$4
    for retry in 1 2 3 4 5; do 
	(cd $BENCHROOT/$BENCH/ubiprof; make run)
	if [ $? = 0 ] ; then break 
	else 
	    (cd $BENCHROOT/$BENCH/$EXECDIR; rm overhead.out) 
	fi 
    done 
    #mv $BENCHROOT/$BENCH/$EXECDIR/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
    mv $BENCHROOT/$BENCH/$EXECDIR/prof.out $DATADIR/prof-$HOSTNAME-$BENCH-$ROUND-$FUNC.out
    mv $BENCHROOT/$BENCH/$EXECDIR/statistics.out $DATADIR/stat-$HOSTNAME-$BENCH-$ROUND-$FUNC.out
}

# ground truth runs
# -------------------

#ubiprof parameters 
export PROFILER_TYPE=FIXED_BACKOFF
export SAMPLE_SIZE=200000000000

: << 'EOF'

for func in $H264_INSTR_FUNCS ; do 
    set_profiled_func H264_FUNCS $func
    build_it h264ref-9.3 ;
    for round in 1 2 3 ; do 
	run_it h264ref-9.3 $round $func    
    done 
done

for func in $HMMER_INSTR_FUNCS ; do 
    set_profiled_func HMMER_FUNCS $func
    build_it hmmer ;
    for round in 1 2 3 ; do 
	run_it hmmer $round $func       
    done 
done

for func in $LBM_INSTR_FUNCS ; do 
    set_profiled_func LBM_FUNCS $func
    build_it lbm ;
    for round in 1 2 3 ; do 
     	run_it lbm $round $func       
    done 
done 

for func in $PERL_INSTR_FUNCS ; do 
    set_profiled_func PERL_FUNCS $func
    build_it perl-5.8.7 ;
    for round in 1 2 3 ; do 
	run_it perl-5.8.7 $round $func       
    done 
done 

for func in $SJENG_INSTR_FUNCS ; do
    set_profiled_func SJENG_FUNCS $func
    build_it sjeng ;
    for round in 1 2 3 ; do 
	run_it sjeng $round $func       
    done 
done

for func in $BZIP_INSTR_FUNCS ; do 
    set_profiled_func BZIP_FUNCS $func
    build_it bzip-1.0.3 ;
    for round in 1 2 3 ; do 
 	run_it bzip-1.0.3 $round $func       
    done 
done


EOF

    #Those parallel benchmarks need special attention! 

    #blackscholes 
for func in $BLACKSCHOLES_INSTR_FUNCS ; do 
    set_profiled_func BLACKSCHOLES_FUNCS $func
    build_it blackscholes ; 
    for round in 1 2 3 ; do 
	run_it_par blackscholes src $round $func    
    done 
done 


: << 'EOF'

    #fluid
for func in $FLUID_INSTR_FUNCS ; do 
    set_profiled_func FLUID_FUNCS $func
    build_it fluid ; 
    for round in 1 2 3 ; do 
	run_it_par fluid src $round $func    	
    done 
done 


    #hull
for func in $HULL_INSTR_FUNCS ; do
    set_profiled_func HULL_FUNCS $func
    build_it hull ; 
    for round in 1 2 3 ; do 
	run_it_par hull quickHull $round $func    
    done 
done

    #nbody 
for func in $NBODY_INSTR_FUNCS ; do 
    set_profiled_func NBODY_FUNCS $func 
    build_it nbody ; 
    # for round in 1 2 3 ; do 
    for round in 1 ; do 
	run_it_par nbody BarnesHut $round $func    
    done 
done 



#adaptive runs
# --------------

function run_it_adaptive { 
    BENCH=$1
    ROUND=$2 
    TARGOH=$3

    (cd $BENCHROOT/$BENCH/ubiprof; make run) 
    # will not work for the parallel benches!! 
    # that use a different directory structure (My bad idea) 
    #mv $BENCHROOT/$BENCH/instrumented/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
    mv $BENCHROOT/$BENCH/instrumented/prof.out $DATADIR/adaptive-prof-$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
    mv $BENCHROOT/$BENCH/instrumented/statistics.out $DATADIR/adaptive-stat-$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
}

function run_it_par_adaptive { 
    BENCH=$1
    EXECDIR=$2
    ROUND=$3
    TARGOH=$4
    for retry in 1 2 3 4 5; do 
	(cd $BENCHROOT/$BENCH/ubiprof; make run)
	if [ $? = 0 ] ; then break 
	else 
	    (cd $BENCHROOT/$BENCH/$EXECDIR; rm overhead.out) 
	fi 
    done 
    #mv $BENCHROOT/$BENCH/$EXECDIR/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
    mv $BENCHROOT/$BENCH/$EXECDIR/prof.out $DATADIR/adaptive-prof-$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
    mv $BENCHROOT/$BENCH/$EXECDIR/statistics.out $DATADIR/adaptive-stat-$HOSTNAME-$BENCH-$ROUND-$TARGOH.out
}

#ubiprof parameters 
export PROFILER_TYPE=MINIMAL_ADAPTIVE
unset SAMPLE_SIZE

build_it_regular h264ref-9.3 ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_adaptive h264ref-9.3 $round $overhead    
    done 
done

build_it_regular perl-5.8.7 ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_adaptive perl-5.8.7 $round $overhead    
    done 
done

build_it_regular bzip-1.0.3 ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_adaptive bzip-1.0.3 $round $overhead    
    done 
done

build_it_regular hmmer ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_adaptive hmmer $round $overhead    
    done 
done

build_it_regular sjeng ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_adaptive sjeng $round $overhead    
    done 
done


build_it_regular fluid ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_par_adaptive fluid $round $overhead    
    done 
done

build_it_regular nbody ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_par_adaptive nbody $round $overhead    
    done 
done

build_it_regular hull ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_par_adaptive hull $round $overhead    
    done 
done


build_it_regular blackscholes ;
for overhead in 3 5 10 ; do 
    for round in 1 2 3 ; do 
	run_it_par_adaptive blackscholes $round $overhead    
    done 
done


EOF

: << 'EOF'
#sampling runs
#--------------

function run_it_sampling { 
    BENCH=$1
    ROUND=$2 

    (cd $BENCHROOT/$BENCH/ubiprof; make run) 
    # will not work for the parallel benches!! 
    # that use a different directory structure (My bad idea) 
    #mv $BENCHROOT/$BENCH/instrumented/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/instrumented/prof.out $DATADIR/sampling-prof-$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/instrumented/statistics.out $DATADIR/sampling-stat-$HOSTNAME-$BENCH-$ROUND.out
}

function run_it_par_sampling { 
    BENCH=$1
    EXECDIR=$2
    ROUND=$3
    for retry in 1 2 3 4 5; do 
	(cd $BENCHROOT/$BENCH/ubiprof; make run)
	if [ $? = 0 ] ; then break 
	else 
	    (cd $BENCHROOT/$BENCH/$EXECDIR; rm overhead.out) 
	fi 
    done 
    #mv $BENCHROOT/$BENCH/$EXECDIR/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/$EXECDIR/prof.out $DATADIR/sampling-prof-$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/$EXECDIR/statistics.out $DATADIR/sampling-stat-$HOSTNAME-$BENCH-$ROUND.out
}

#ubiprof parameters 
export PROFILER_TYPE=MINIMAL_SAMPLING
unset SAMPLE_SIZE

build_it_regular h264ref-9.3 ;
for round in 1 2 3 ; do 
	run_it_sampling h264ref-9.3 $round 
done 

build_it_regular bzip-1.0.3 ;
for round in 1 2 3 ; do 
	run_it_sampling bzip-1.0.3 $round 
done 

build_it_regular perl-5.8.7 ;
for round in 1 2 3 ; do 
	run_it_sampling perl-5.8.7 $round 
done 

build_it_regular hmmer ;
for round in 1 2 3 ; do 
	run_it_sampling hmmer $round 
done 

build_it_regular lbm ;
for round in 1 2 3 ; do 
	run_it_sampling lbm $round 
done 

build_it_regular sjeng ;
for round in 1 2 3 ; do 
	run_it_sampling sjeng $round 
done 


build_it_regular fluid ;
for round in 1 2 3 ; do 
	run_it_par_sampling fluid $round 
done 

build_it_regular nbody ;
for round in 1 2 3 ; do 
	run_it_par_sampling nbody $round 
done 

build_it_regular blackscholes ;
for round in 1 2 3 ; do 
	run_it_par_sampling blackscholes $round 
done 

build_it_regular hull;
for round in 1 2 3 ; do 
	run_it_par_sampling hull $round 
done

EOF

: << 'EOF'
#backoff runs
#--------------

function run_it_backoff { 
    BENCH=$1
    ROUND=$2 

    (cd $BENCHROOT/$BENCH/ubiprof; make run) 
    # will not work for the parallel benches!! 
    # that use a different directory structure (My bad idea) 
    #mv $BENCHROOT/$BENCH/instrumented/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/instrumented/prof.out $DATADIR/backoff-prof-$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/instrumented/statistics.out $DATADIR/backoff-stat-$HOSTNAME-$BENCH-$ROUND.out
}

function run_it_par_backoff { 
    BENCH=$1
    EXECDIR=$2
    ROUND=$3
    for retry in 1 2 3 4 5; do 
	(cd $BENCHROOT/$BENCH/ubiprof; make run)
	if [ $? = 0 ] ; then break 
	else 
	    (cd $BENCHROOT/$BENCH/$EXECDIR; rm overhead.out) 
	fi 
    done 
    #mv $BENCHROOT/$BENCH/$EXECDIR/overhead.out $DATADIR/$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/$EXECDIR/prof.out $DATADIR/backoff-prof-$HOSTNAME-$BENCH-$ROUND.out
    mv $BENCHROOT/$BENCH/$EXECDIR/statistics.out $DATADIR/backoff-stat-$HOSTNAME-$BENCH-$ROUND.out
}

#ubiprof parameters 
export PROFILER_TYPE=MINIMAL_BACKOFF
export SAMPLE_SIZE=100000

build_it_regular h264ref-9.3 ;
for round in 1 2 3 ; do 
	run_it_backoff h264ref-9.3 $round 
done 

build_it_regular bzip-1.0.3 ;
for round in 1 2 3 ; do 
	run_it_backoff bzip-1.0.3 $round 
done 

build_it_regular perl-5.8.7 ;
for round in 1 2 3 ; do 
	run_it_backoff perl-5.8.7 $round 
done 

build_it_regular hmmer ;
for round in 1 2 3 ; do 
	run_it_backoff hmmer $round 
done 

build_it_regular lbm ;
for round in 1 2 3 ; do 
	run_it_backoff lbm $round 
done 

build_it_regular sjeng ;
for round in 1 2 3 ; do 
	run_it_backoff sjeng $round 
done 

build_it_regular fluid ;
for round in 1 2 3 ; do 
	run_it_par_backoff fluid $round 
done 

build_it_regular nbody ;
for round in 1 2 3 ; do 
	run_it_par_backoff nbody $round 
done 

build_it_regular blackscholes ;
for round in 1 2 3 ; do 
	run_it_par_backoff blackscholes $round 
done 

build_it_regular hull;
for round in 1 2 3 ; do 
	run_it_par_backoff hull $round 
done 

EOF


#!/bin/bash

# Define paths
AF_trk="/Users/neuro-240/Documents/BIL_and_GIN_Visit/AF_trk_files/"
CC_trk="/Users/neuro-240/Documents/BIL_and_GIN_Visit/CC_trk_files/"

# Function to extract subject IDs from file names
extract_subject_ids() {
    local tract_dir="$1"
    subjects=()
    local file_list=("$tract_dir"*)
    for file in "${file_list[@]}"; do
        filename=$(basename "$file")
        subjID=${filename%%__*}
        subjects+=("$subjID")
    done
}

# Function to compute centroids for tracts
compute_centroids() {
    local tract_dir="$1"
    local tract_type="$2"
    local centroid_prefix="$3"
    for subjID in "${subjects[@]}"; do
        for side in "L" "R"; do
            trk_path="${tract_dir}/${subjID}__${tract_type}_${side}_mni_space.trk"
            centroid_path="${tract_dir}/${subjID}__${tract_type}_${side}_mni_space_${centroid_prefix}.trk"
            scil_compute_centroid.py "$trk_path" "$centroid_path"
            echo "Computed centroid for AF tract file: $subjID"
        done
    done
}

# Function to divide trk files
divide_trk() {
    local trk_folder="$1"
    local subjID="$2"
    local structure="$3"
    local side="${4:-}"  # Set default value to empty string if side is not provided

    # Construct file paths based on whether side is provided or not
    if [ -n "$side" ]; then
        input_trk="${trk_folder}${subjID}__${structure}_${side}_mni_space.trk"
        centroid_trk="${trk_folder}${subjID}__${structure}_${side}_mni_space_centroid.trk"
        output_prefix="${trk_folder}${subjID}_${structure}_divided_${side}"
    else
        input_trk="${trk_folder}${subjID}__${structure}_mni_space.trk"
        centroid_trk="${trk_folder}${subjID}__${structure}_mni_space_centroid.trk"
        output_prefix="${trk_folder}${subjID}_${structure}_divided"
    fi

    echo "Input TRK: $input_trk"
    echo "Centroid TRK: $centroid_trk"
    echo "Output Prefix: $output_prefix"

    # Verify if the input file exists before running the command
    if [ ! -f "$input_trk" ]; then
        echo "Error: Input file $input_trk does not exist"
        return
    fi

    scil_compute_bundle_voxel_label_map.py "$input_trk" "$centroid_trk" --nb_pts 5 "$output_prefix" -f
}



# Process AF tracts
process_AF_tracts() {
    extract_subject_ids "$AF_trk"
    subjects=($(echo "${subjects[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))
    compute_centroids "$AF_trk" "AF" "centroid"
}

# Process CC tracts
process_CC_tracts() {
    extract_subject_ids "$CC_trk"
    subjects=($(echo "${subjects[@]}" | tr ' ' '\n' | sort -u | tr '\n' ' '))
    for subjID in "${subjects[@]}"; do
        for cc_file in "${CC_trk}/${subjID}"__cc_homotopic_*.trk; do
            centroid_path="${cc_file%.trk}_centroid.trk"
            scil_compute_centroid.py "$cc_file" "$centroid_path"
            echo "Computed centroid for CC tract file: $cc_file"
        done
    done
}

# Main Script

# Process AF tracts
process_AF_tracts

# Process CC tracts
process_CC_tracts

# Divide AF tracts
for file_path in "${AF_trk}"*__af_*.trk; do
    file_name=$(basename "$file_path")
    subjID="${file_name%%__af_*}"  
    for side in L R; do
        divide_trk "$AF_trk" "$subjID" "af" "$side"
    done
done

# Divide CC tracts
for file_path in "${CC_trk}"*__cc_homotopic_*.trk; do
    subjID=$(basename "$file_path" | cut -d'_' -f1)
    divide_trk "$CC_trk" "$subjID" "cc_homotopic_SCC"
    divide_trk "$CC_trk" "$subjID" "cc_homotopic_BCC"
    divide_trk "$CC_trk" "$subjID" "cc_homotopic_GCC"
done

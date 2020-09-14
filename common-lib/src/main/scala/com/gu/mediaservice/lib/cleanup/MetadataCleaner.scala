package com.gu.mediaservice.lib.cleanup

import com.gu.mediaservice.model.{Image, ImageMetadata}

trait MetadataCleaner {
  def clean(metadata: ImageMetadata): ImageMetadata
}

class MetadataCleaners(creditBylineMap: Map[String, List[String]]) extends ImageProcessor {

  val attrCreditFromBylineCleaners = creditBylineMap.map { case (credit, bylines) =>
    AttributeCreditFromByline(bylines, credit)
  }

  val allCleaners: List[MetadataCleaner] = List(
    CleanRubbishLocation,
    StripCopyrightPrefix,
    RedundantTokenRemover,
    BylineCreditReorganise,
    UseCanonicalGuardianCredit,
    ExtractGuardianCreditFromByline
  ) ++ attrCreditFromBylineCleaners ++ List(
    CountryCode,
    GuardianStyleByline,
    CapitaliseByline,
    InitialJoinerByline,
    CapitaliseCountry,
    CapitaliseState,
    CapitaliseCity,
    CapitaliseSubLocation,
    DropRedundantTitle,
    PhotographerRenamer
  )

  def apply(inputImage: Image): Image = inputImage.copy(
    metadata = allCleaners.foldLeft(inputImage.metadata) {
      case (metadata, cleaner) => cleaner.clean(metadata)
    }
  )
}

// By vague order of importance:

// TODO: strip location+date prefix from description
// TODO: strip credit suffix from description
// TODO: strip (extract?) country + tags suffix from description

// TODO: strip (?) numbers or crappy acronyms as byline
// TODO: multiple country names (SWITZERLAND SCHWEIZ SUISSE, HKG, CHN) to clean name

// TODO: ignore crappy "keywords" (:rel:d:bm:LM1EAAO112401)

// TODO: unique keywords

// Ingested metadata:

// TODO: record Date Created or Date/Time Original
// TODO: ignore Unknown tags from fileMetadata

// TODO: artist (vs byline)?

//
// Copyright (C) 2013-2021 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

Form
{
  DropDown
  {
    name: "test"
    id:   test
    indexDefaultValue: 0
    label: qsTr("Statistical test:")
    values: [
      { label: "Independent Samples T-Test", value: "ttest_independent" },
      { label: "Paired Samples T-Test",  value: "ttest_paired"          },
      { label: "One Sample T-Test",  value: "ttest_onesample"           },
      { label: "One Sample Z-Test",  value: "ztest_onesample"           },
      { label: "One Sample Proportion Test",  value: "test_oneprop"     },
      { label: "Two Samples Proportion Test",  value: "test_twoprop"    },
      { label: "One Sample Variance Ratio Test",  value: "test_onevar" },
      { label: "Two Samples Variance Ratio Test",  value: "test_twovar"}
      //{ label: "One Sample Poisson Rate Test",  value: "test_onepois"   },
      //{ label: "Two Samples Poisson Rate Test",  value: "test_twopois"  }
      //{ label: "ANOVA",  value: "anova" }
    ]
  }

	Section
	{
		expanded: true
    visible: test.currentValue !== 'anova'
		title: qsTr("Parameters (t-tests)")

    Group
		{
			Layout.columnSpan: 2

			Group
			{
				columns: 2

        Text { text: qsTr("I want to calculate the ...") }
        DropDown
        {
          name: "calc"
          id:   calc
          indexDefaultValue: 0
          label: qsTr("")
          values: [
            { label: "Sample Size N", value: "n"},
            { label: "Power",  value: "power" },
            { label: "Effect size",  value: "es"}
          ]
        }

        Text {
          text: qsTr("Direction of the effect:")
          visible: (test.currentIndex == 6 || test.currentIndex == 7) && calc.currentIndex == 2
          enabled: calc.currentIndex == 2
        }
				DropDown 
        {
          id: direction
          name: "directionOfEffect"
          label: qsTr("")
          visible: (test.currentIndex == 6 || test.currentIndex == 7) && calc.currentIndex == 2
          enabled: calc.currentIndex == 2
          values: [
            { label: "\u03C1 > 1", value: "greater"},
            { label: "\u03C1 < 1",  value: "less" }
          ]
        }

        Text {
          text: (test.currentIndex == 4) ? qsTr("Hypothesized proportion") : qsTr("Baseline proportion")
          visible: test.currentIndex == 4 || test.currentIndex == 5
        }
				DoubleField {
          id: p0
          name: "p0"
          label: (test.currentIndex == 4) ? qsTr("p₀") : qsTr("p₂")
          min: 0.01
          max: 0.99
          defaultValue: 0.5
          visible: test.currentIndex == 4 || test.currentIndex == 5
        }

        Text {
          text: qsTr("Comparison proportion")
          visible: test.currentIndex == 4 || test.currentIndex == 5
          enabled: calc.currentIndex != 2
        }
				DoubleField {
          id: p1
          name: "p1"
          label: qsTr("p₁")
          min: 0.01
          max: 0.99
          defaultValue: 0.6
          visible: test.currentIndex == 4 || test.currentIndex == 5
          enabled: calc.currentIndex != 2
        }

        Text {
          text: qsTr("Minimal effect size of interest:")
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 2 || test.currentIndex == 3
          enabled: calc.currentIndex != 2
        }
				DoubleField {
          id: es
          name: "es"
          label: qsTr("|δ|")
          defaultValue: 0.5
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 2 || test.currentIndex == 3
          enabled: calc.currentIndex != 2
        }

				Text {
          text: qsTr("Minimal effect size of interest:")
          visible: test.currentIndex == 6 || test.currentIndex == 7
          enabled: calc.currentIndex != 2
        }
				DoubleField {
          id: rho
          name: "rho"
          label: (test.currentIndex == 7) ? qsTr("\u03C1 (\u03C3\u2081\u00B2/\u03C3\u2082\u00B2)") : qsTr("\u03C1 (\u03C3\u2081\u00B2/\u03C3\u2080\u00B2)")
          defaultValue: 2
          visible: test.currentIndex == 6 || test.currentIndex == 7
          enabled: calc.currentIndex != 2
        }

				Text {
          text: qsTr("Minimal desired power:")
          enabled: calc.currentIndex != 1
        }
				DoubleField {
          id: power
          name: "power"
          label: qsTr("(1-β)")
          min: 0.1
          max: 0.999
          defaultValue: 0.9
          enabled: calc.currentIndex != 1
        }

        // No groups in single sample t-test
				Text {
          text: qsTr("Sample size:")
          visible: test.currentIndex == 2 || test.currentIndex == 3 || test.currentIndex == 4 || test.currentIndex == 6 || test.currentIndex == 8
          enabled: calc.currentIndex != 0
        }
        Text {
          text: qsTr("Sample size per group:")
          visible: test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 5 || test.currentIndex == 7 || test.currentIndex == 9
          enabled: calc.currentIndex != 0
        }
				IntegerField {
          id: n
          name: "n"
          label: qsTr("N")
          min: 2
          defaultValue: 20
          enabled: calc.currentIndex != 0
        }

        Text { text: qsTr("Type I error rate:") }
				DoubleField {
          id: alpha
          name: "alpha"
          label: qsTr("α")
          min: 0
          defaultValue: 0.05
        }

        // No sample size ratio in single sample t-test
        Text {
          text: qsTr("Sample size ratio:")
          visible: test.currentIndex == 0 || test.currentIndex == 5 || test.currentIndex == 7 || test.currentIndex == 9
        }
				DoubleField {
          id: n_ratio
          name: "n_ratio"
          label: qsTr("N₁/N₂")
          min: 0
          defaultValue: 1
          visible: test.currentIndex == 0 || test.currentIndex == 5 || test.currentIndex == 7 || test.currentIndex == 9
        }

        Text { text: qsTr("Alternative Hypothesis:") }
        DropDown
        {
          name: "alt"
          id:   alt
          indexDefaultValue: 0
          label: qsTr("")
          values: (test.currentIndex == 0 || test.currentIndex == 1 || test.currentIndex == 2 || test.currentIndex == 3) ?
          [
            { label: "Two-sided", value: "two.sided"},
            { label: "One-sided",  value: "greater" }
          ] :
          [
            { label: "Two-sided", value: "two.sided"},
            { label: "Less (One-sided)",  value: "less" },
            { label: "Greater (One-sided)",  value: "greater"}
          ]
        }

			}

		}
	}

  // TODO: ANOVA Section

  Section
	{
		expanded: true
		title: qsTr("Display")

    DropDown
        {
          name: "esType"
          id:   esType
          indexDefaultValue: 0
          label: qsTr("Effect size type")
          values: [
            { label: "Cohen's h", value: "h"}
          ]
          visible: false
        }

    CheckBox {
      label: qsTr("Power contour plot")
      id: powerContour
      name: "powerContour"
      checked: true
    }

    CheckBox {
      label: qsTr("Power demonstration")
      id: powerDist
      name: "powerDist"
      checked: false
    }

    CheckBox {
      label: qsTr("Power curve by effect size")
      id: powerCurveES
      name: "powerCurveES"
      checked: true
    }

    CheckBox {
      label: qsTr("Power curve by N")
      id: powerCurveN
      name: "powerCurveN"
      checked: false
    }

    CheckBox {
      label: qsTr("Explanatory text")
      id: text
      name: "text"
      checked: true
    }
  }
}

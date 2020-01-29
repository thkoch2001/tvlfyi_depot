from test_utils import simple_assert


def to_rule_id(x):
    """Transform `x` to the casing expected in a rule ID."""
    return x.lower().replace(' ',
                             '_').replace('(',
                                          '').replace(')',
                                                      '').replace('/', '_')


expected = {
    'Cloaking or Cloaking Suspect': 'cloaking_or_cloaking_suspect',
    'Phishing': 'phishing',
    'System Suspended Advertiser': 'system_suspended_advertiser',
    'Affiliate Spam': 'affiliate_spam',
    'Untrustworthy Behavior': 'untrustworthy_behavior',
    'Payment Fraud': 'payment_fraud',
    'Bad Debt': 'bad_debt',
    'Sleeper': 'sleeper',
    'Gaming': 'gaming',
    'Counterfeit': 'counterfeit',
    'Coupon Abuse': 'coupon_abuse',
    'Fraud chargeback': 'fraud_chargeback',
    'Friendly chargeback': 'friendly_chargeback',
    'Customer service chargeback': 'customer_service_chargeback',
    'Delinquency (wont pay)': 'delinquency_wont_pay',
    'Direct Debit Abuse/Suspect': 'direct_debit_abuse_suspect',
    'Delinquent Account Abuse': 'delinquent_account_abuse',
    'Billing Customer Shutdown': 'billing_customer_shutdown',
    'Violations Across Multiple Accounts(VAMA)':
    'violations_across_multiple_accountsvama',
    'Risk Mitigation Daily Spend Limit Raise':
    'risk_mitigation_daily_spend_limit_raise',
    'Review Account Under Review': 'review_account_under_review',
    'Runaway Spenders Daily Spend Limit': 'runaway_spenders_daily_spend_limit',
    'Double Serving': 'double_serving',
    'Gaining an Unfair Advantage': 'gaining_an_unfair_advantage',
    'Office of Foreign Assets Control (OFAC) Sanctions':
    'office_of_foreign_assets_control_ofac_sanctions',
    'Terms of service violation': 'terms_of_service_violation',
    'Spam': 'spam',
    'Sexually Explicit Content': 'sexually_explicit_content',
    'Illegal drugs (Dangerous Products)': 'illegal_drugs_dangerous_products',
    'Hate speech': 'hate_speech',
    'Harrassment': 'harrassment',
    'Malicious Or Unwanted Software': 'malicious_or_unwanted_software',
    'Online Gambling': 'online_gambling',
    'Social Casino Games': 'social_casino_games',
    'Online Pharmacy Certification Required':
    'online_pharmacy_certification_required',
    'Copyrights': 'copyrights',
    'Addiction Services': 'addiction_services',
    'Elections': 'elections',
    'Unwanted Software': 'unwanted_software',
    'Event Ticket Reseller': 'event_ticket_reseller',
    'Cryptocurrency': 'cryptocurrency',
    'Complex Speculative Financial Policy':
    'complex_speculative_financial_policy',
    'US Elections Ads (New York Only)': 'us_elections_ads_new_york_only',
    'Alcohol Information': 'alcohol_information',
    'Inappropriate Content': 'inappropriate_content',
    'Adult Content': 'adult_content',
    'final reason blogger suspension': 'final_reason_blogger_suspension',
    'final reason calendar suspension': 'final_reason_calendar_suspension',
    'final reason writely suspension': 'final_reason_writely_suspension',
    'final reason groups reloaded suspension':
    'final_reason_groups_reloaded_suspension',
    'final reason gplus suspension': 'final_reason_gplus_suspension',
    'Terrorist Content': 'terrorist_content',
    'Underage account': 'underage_account',
    'Terrorist Content': 'terrorist_content',
    'Elections': 'elections',
    'Policy System Suspension': 'policy_system_suspension',
}

for x, y in expected.items():
    simple_assert(to_rule_id(x), y, 'to_rule_id')
